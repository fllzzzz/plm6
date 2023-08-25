<template>
  <common-drawer
    ref="drawerRef"
    title="变更记录"
    :close-on-click-modal="false"
    v-model="visible"
    direction="rtl"
    :before-close="handleClose"
    custom-class="invoice-record"
    size="80%"
  >
    <template #content>
      <div class="head-container">
        <common-radio-button
          v-model="query.checkStatus"
          :options="auditTypeEnum.ENUM"
          showOptionAll
          type="enum"
          size="small"
          class="filter-item"
          @change="fetchList"
        />
        <el-input
          v-model="query.licensePlate"
          placeholder="车牌号搜索"
          class="filter-item"
          style="width: 200px"
          size="small"
          clearable
        />
        <el-input
          v-model="query.purchaseSn"
          placeholder="采购单"
          class="filter-item"
          style="width: 200px"
          size="small"
          clearable
        />
        <common-button class="filter-item" size="small" type="success" icon="el-icon-search" @click.stop="fetchList">搜索</common-button>
        <common-button class="filter-item" size="small" type="warning" icon="el-icon-refresh" @click.stop="resetSubmit">重置</common-button>
      </div>
      <common-table ref="tableRef" :data="list" v-loading="tableLoading"  :data-format="dataFormat" :max-height="maxHeight-180">
        <el-table-column prop="licensePlate" label="车牌号" align="center" show-overflow-tooltip />
        <el-table-column prop="loadingWeight" label="装载重量（吨）" align="center" show-overflow-tooltip />
        <el-table-column prop="freight" label="运输费" align="center" show-overflow-tooltip>
          <template #default="{ row }">
            <cell-change-preview :old="row.oldFreight" :new="row.newFreight" />
          </template>
        </el-table-column>
        <el-table-column prop="purchaseSn" label="采购订单号" align="center" min-width="100" show-overflow-tooltip />
        <el-table-column prop="inboundSn" label="关联入库单" align="center" show-overflow-tooltip />
        <el-table-column key="createUserName" prop="createUserName" label="创建人" align="center" width="100" show-overflow-tooltip />
        <el-table-column key="createTime" prop="createTime" label="创建时间"  align="center"  width="100" show-overflow-tooltip />
        <el-table-column key="checkUserName" prop="checkUserName" label="审核人" align="center" width="100" show-overflow-tooltip />
        <el-table-column key="checkTime"  prop="checkTime" label="审核时间"  align="center"  width="100" show-overflow-tooltip />
        <el-table-column label="审核状态" width="150" align="center" fixed="right">
          <template #default="{ row }">
            <template v-if="row.checkStatus===auditTypeEnum.AUDITING.V && checkPermission(permission.freightChangeAudit)">
              <el-popconfirm
                confirm-button-text="确定"
                cancel-button-text="取消"
                title="确定审核通过吗?"
                @confirm="passConfirm(row.id,auditTypeEnum.PASS.V)"
              >
                <template #reference>
                  <common-button type="success" size="mini">通过</common-button>
                </template>
              </el-popconfirm>
              <el-popconfirm
                confirm-button-text="确定"
                cancel-button-text="取消"
                title="确定驳回吗?"
                @confirm="passConfirm(row.id,auditTypeEnum.REJECT.V)"
              >
                <template #reference>
                  <common-button type="danger" size="mini">驳回</common-button>
                </template>
              </el-popconfirm>
            </template>
            <template v-else>
              <el-tag :type="auditTypeEnum.V[row.checkStatus]?.T" size="medium">{{auditTypeEnum.VL[row.checkStatus]}}</el-tag>
            </template>
          </template>
        </el-table-column>
      </common-table>
      <!--分页组件-->
      <el-pagination
        :total="total"
        :current-page="queryPage.pageNumber"
        :page-size="queryPage.pageSize"
        style="margin-top: 8px"
        layout="total, prev, pager, next, sizes"
        @size-change="handleSizeChange"
        @current-change="handleCurrentChange"
      />
      <mPreview v-model="previewVisible" :modified-data="modifiedData" @success="fetchList" />
    </template>
  </common-drawer>
</template>

<script setup>
import { ElMessage } from 'element-plus'
import { feeChangeRecord, feeChangeAudit } from '@/api/supply-chain/logistics-payment-manage/jd-logistics-record-ledger'
import { ref, defineEmits, defineProps, watch, computed } from 'vue'

import { auditTypeEnum } from '@enum-ms/contract'
import cellChangePreview from '@comp-common/cell-change-preview'

import checkPermission from '@/utils/system/check-permission'
import useVisible from '@/composables/use-visible'
import useMaxHeight from '@compos/use-max-height'
import usePagination from '@compos/use-pagination'
import mPreview from './preview'

const emit = defineEmits(['update:modelValue'])

const props = defineProps({
  modelValue: {
    type: Boolean,
    require: true
  },
  permission: {
    type: Object,
    default: () => {}
  },
  supplierId: {
    type: [Number, String],
    default: undefined
  }
})

const { visible, handleClose } = useVisible({ emit, props })
const { handleSizeChange, handleCurrentChange, total, setTotalPage, queryPage } = usePagination({ fetchHook: fetchList })
const query = ref({})

// 请求参数
const params = computed(() => {
  // 汇总列表
  return {
    supplierId: props.supplierId,
    ...query.value
  }
})

watch(
  visible,
  (val) => {
    if (val) {
      fetchList()
    }
  },
  { immediate: true }
)

const tableRef = ref()
const list = ref([])
const drawerRef = ref()
const tableLoading = ref(false)
const modifiedData = ref([])
const previewVisible = ref(false)

const { maxHeight } = useMaxHeight(
  {
    mainBox: '.invoice-record',
    extraBox: '.el-drawer__header',
    wrapperBox: '.el-drawer__body',
    paginate: true,
    minHeight: 300,
    navbar: false,
    clientHRepMainH: true
  },
  drawerRef
)

const dataFormat = ref([
  ['createTime', ['parse-time', '{y}-{m}-{d}']],
  ['checkTime', ['parse-time', '{y}-{m}-{d}']]
])

// 重置搜索
function resetSubmit() {
  query.value = {}
  fetchList()
}

// 获取物流记录
async function fetchList() {
  let _list = []
  tableLoading.value = true
  try {
    const { content = [], totalElements } = await feeChangeRecord({ ...params.value, ...queryPage })
    content.map(v => {
      v.loadingWeight = v.loadingWeight ? (v.loadingWeight / 1000).toFixed(2) : 0
    })
    _list = content
    setTotalPage(totalElements)
  } catch (error) {
    console.log('获取物流记录失败', error)
  } finally {
    list.value = _list
    tableLoading.value = false
  }
}

async function passConfirm(id, checkStatus) {
  try {
    await feeChangeAudit({ id, checkStatus })
    ElMessage.success('审核已' + auditTypeEnum.VL[checkStatus])
    fetchList()
    emit('success')
  } catch (e) {
    console.log('审核失败', e)
  }
}
</script>
<style lang="scss" scoped>
.clickable {
  cursor: pointer;
  color:#409eff;
}
</style>
