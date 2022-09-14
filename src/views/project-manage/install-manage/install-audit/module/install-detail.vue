<template>
  <common-drawer
    ref="drawerRef"
    :title="showType==='detail'?'安装详情':'安装审核'"
    :close-on-click-modal="false"
    v-model="visible"
    direction="rtl"
    :before-close="handleClose"
    custom-class="invoice-record"
    size="80%"
  >
    <template #titleAfter>
      <el-tag type="warning" size="medium">分包公司：{{detailInfo.supplierName}}</el-tag>
      <el-tag v-if="showType==='audit'" size="medium">待审核</el-tag>
    </template>
    <template #titleRight>
      <template v-if="showType==='audit' && list.length">
        <el-popconfirm title="确定提交吗？" @confirm="confirmSubmit">
          <template #reference>
            <common-button type="success" size="mini">提交</common-button>
          </template>
        </el-popconfirm>
      </template>
    </template>
    <template #content>
      <common-radio-button
        v-if="showType==='detail'"
        v-model="auditStatus"
        :options="auditTypeEnum.ENUM"
        showOptionAll
        :optionAllValue="undefined"
        type="enum"
        class="filter-item"
        style="margin-bottom:10px;"
        @change="fetchList(auditStatus)"
      />
      <common-table :data="list" v-loading="tableLoading" show-summary :summary-method="getSummaries" :max-height="maxHeight" return-source-data :showEmptySymbol="false">
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column key="createTime" prop="createTime" label="申报时间" align="center" width="140">
          <template v-slot="scope">
            <span>{{parseTime(scope.row.createTime,'{y}-{m}-{d} {h}:{i}:{s}')}}</span>
          </template>
        </el-table-column>
        <el-table-column key="monomer.name" prop="monomer.name" label="单体" align="center" show-overflow-tooltip />
        <el-table-column key="area.name" prop="area.name" label="区域" align="center" show-overflow-tooltip />
        <el-table-column key="name" prop="name" label="名称" align="center" show-overflow-tooltip />
        <el-table-column key="serialNumber" prop="serialNumber" label="编号" align="center" show-overflow-tooltip />
        <el-table-column key="specification" prop="specification" label="规格" align="center" show-overflow-tooltip />
        <el-table-column key="reportQuantity" prop="reportQuantity" label="上报数" align="center">
          <template v-slot="scope">
            <span>{{scope.row.reportQuantity}}<span style="margin-left:3px;">{{ scope.row.measureUnit }}</span></span>
          </template>
        </el-table-column>
        <el-table-column key="reportMete" prop="reportMete" label="上报量" align="center">
          <template v-slot="scope">
            <span>{{scope.row.reportMete}}<span style="margin-left:3px;">{{ scope.row.accountingUnit }}</span></span>
          </template>
        </el-table-column>
        <el-table-column key="actualQuantity" prop="actualQuantity" label="审核数" align="center" min-width="150">
          <template v-slot="scope">
            <el-input-number
              v-if="showType==='audit'"
              v-model.number="scope.row.actualQuantity"
              :min="0"
              :max="scope.row.reportQuantity"
              :step="1"
              :precision="0"
              placeholder="请填写"
              controls-position="right"
              style="width:120px;"
              @change="quantityChange(scope.row)"
            />
            <span v-else> {{scope.row.actualQuantity || '-'}} </span>
            <span style="margin-left:3px;">{{scope.row.measureUnit}}</span>
          </template>
        </el-table-column>
        <el-table-column key="actualMete" prop="actualMete" label="审核量" align="center">
          <template v-slot="scope">
           <span>{{ scope.row.actualMete || '-' }}<span style="margin-left:3px;">{{ scope.row.accountingUnit }}</span></span>
          </template>
        </el-table-column>
        <el-table-column key="auditStatus" prop="auditStatus" label="状态" align="center" width="80" v-if="showType==='detail'">
          <template v-slot="scope">
            <el-tag :type="scope.row.auditStatus===auditTypeEnum.PASS.V?'success':(scope.row.auditStatus===auditTypeEnum.REJECT.V?'warning':'')">{{ auditTypeEnum.VL[scope.row.auditStatus] }}</el-tag>
          </template>
        </el-table-column>
      </common-table>
      <el-pagination
        :total="total"
        :current-page="queryPage.pageNumber"
        :page-size="queryPage.pageSize"
        style="margin-top: 8px"
        layout="total, prev, pager, next, sizes"
        @size-change="handleSizeChange"
        @current-change="handleCurrentChange"
      />
    </template>
  </common-drawer>
</template>

<script setup>
import { installDetail, installAuditSave } from '@/api/project-manage/install-manage/install-audit'
import { ref, defineEmits, defineProps, watch, computed } from 'vue'

import { parseTime } from '@/utils/date'
import { isNotBlank } from '@data-type/index'
import { auditTypeEnum } from '@enum-ms/contract'
import { tableSummary } from '@/utils/el-extra'
import { ElNotification, ElMessage } from 'element-plus'

import useVisible from '@/composables/use-visible'
import useMaxHeight from '@compos/use-max-height'
import usePagination from '@compos/use-pagination'

const emit = defineEmits(['update:modelValue'])
const auditStatus = ref()
const props = defineProps({
  modelValue: {
    type: Boolean,
    require: true
  },
  detailInfo: {
    type: Object,
    default: () => {}
  },
  permission: {
    type: Object,
    default: () => {}
  },
  showType: {
    type: String,
    default: 'detail'
  }
})

const { visible, handleClose } = useVisible({ emit, props })
const { handleSizeChange, handleCurrentChange, total, setTotalPage, queryPage } = usePagination({ fetchHook: fetchList })

// 请求参数
const params = computed(() => {
  const queryObj = {
    supplierId: props.detailInfo.supplierId,
    productType: props.detailInfo.productType,
    projectId: props.detailInfo.projectId,
    yearMonth: props.detailInfo.yearMonth,
    category: props.detailInfo.category || undefined,
    yearMonthDay: props.detailInfo.reportDate ? props.detailInfo.reportDate : undefined
  }
  return props.showType === 'detail' ? { ...queryObj } : { ...queryObj, auditStatus: auditTypeEnum.AUDITING.V }
})

watch(
  visible,
  (val) => {
    auditStatus.value = undefined
    if (val) {
      fetchList()
    }
  }
)

const list = ref([])
const drawerRef = ref()
const tableLoading = ref(false)

const { maxHeight } = useMaxHeight(
  {
    mainBox: '.install-detail',
    extraBox: '.el-drawer__header',
    wrapperBox: '.el-drawer__body',
    paginate: true,
    minHeight: 300,
    navbar: false,
    clientHRepMainH: true
  },
  drawerRef
)

// 合计
function getSummaries(param) {
  const summary = tableSummary(param, {
    props: ['actualQuantity']
  })
  return summary
}

// 获取审核详情
async function fetchList(status) {
  let _list = []
  tableLoading.value = true
  try {
    const { content = [], totalElements } = await installDetail({ auditStatus: status, ...params.value, ...queryPage })
    content.map(v => {
      if (params.value.auditStatus === auditTypeEnum.AUDITING.V) {
        v.actualQuantity = v.reportQuantity
        v.actualMete = v.reportMete
        v.unitMete = v.reportMete / v.reportQuantity
      }
    })
    _list = content
    setTotalPage(totalElements)
  } catch (error) {
    console.log('审核详情', error)
  } finally {
    list.value = [..._list]
    tableLoading.value = false
  }
}

function quantityChange(row) {
  if (row.actualQuantity) {
    row.actualMete = row.unitMete * row.actualQuantity
    row.sourceRow.actualQuantity = row.actualQuantity
  }
}

async function confirmSubmit(val) {
  try {
    const submitData = []
    list.value.map(v => {
      if (isNotBlank(v.actualQuantity)) {
        submitData.push({
          id: v.id,
          actualQuantity: v.actualQuantity
        })
      }
    })
    if (!submitData.length) {
      ElMessage.error('请填写安装审核明细')
      return false
    }
    await installAuditSave(submitData)
    ElNotification({ title: '提交成功', type: 'success' })
    emit('success')
    handleClose()
  } catch (error) {
    console.log('审核', error)
  }
}
</script>
