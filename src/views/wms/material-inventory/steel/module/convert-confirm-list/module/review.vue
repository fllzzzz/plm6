<template>
  <common-drawer
    ref="detailRef"
    append-to-body
    :close-on-click-modal="false"
    :before-close="handleClose"
    v-model="visible"
    title="审核"
    :wrapper-closable="false"
    size="85%"
    custom-class="convert-detail"
  >
    <template #titleAfter>
      <el-tag type="success">条板转换单：{{detail.receiptSerialNumber}}</el-tag>
      <el-tag type="danger" v-if="detail.boolPartyA">甲供</el-tag>
    </template>
    <template #titleRight>
      <!-- 审核按钮 -->
      <review-confirm-button
        v-model:passed-loading="passedLoading"
        v-model:returned-loading="returnedLoading"
        :passed-fn="passed"
        :returned-fn="returned"
      />
    </template>
    <template #content>
      <el-tag effect="plain">原物料信息</el-tag>
      <el-descriptions :column="3" border style="margin:10px 0;">
        <el-descriptions-item label-class-name="contractLabel" label="物料名称">{{ detail.classifyName || '-' }}</el-descriptions-item>
        <el-descriptions-item label-class-name="contractLabel" label="物料编号">{{ detail.serialNumber || '-' }}</el-descriptions-item>
        <el-descriptions-item label-class-name="contractLabel" label="规格">
          {{ detail.specification || '-' }}
        </el-descriptions-item>
        <el-descriptions-item label-class-name="contractLabel" label="厚(mm)*宽(mm)">
          {{ detail.widthThick || '-' }}
        </el-descriptions-item>
        <el-descriptions-item label-class-name="contractLabel" label="品牌">
          {{ detail.brand || '-' }}
        </el-descriptions-item>
        <el-descriptions-item label-class-name="contractLabel" label="颜色">
          {{ detail.color || '-' }}
        </el-descriptions-item>
        <el-descriptions-item label-class-name="contractLabel" label="卷号">
          {{ detail.heatNoAndBatchNo || '-' }}
        </el-descriptions-item>
        <el-descriptions-item label-class-name="contractLabel" label="项目">
          {{ detail.project? projectNameFormatter(detail.project) : '-' }}
        </el-descriptions-item>
        <el-descriptions-item label-class-name="contractLabel" label="仓库">
          {{ detail.warehouse?.name || '-' }}
        </el-descriptions-item>
        <el-descriptions-item label-class-name="contractLabel" label="备注">
          {{ detail.remark || '-' }}
        </el-descriptions-item>
      </el-descriptions>
      <el-divider><span class="title">转换后物料信息</span></el-divider>
      <common-table
        :data="detail.convertDetailDTOList"
        v-loading="loading"
        :data-format="dataFormat"
        show-summary
        :summary-method="getSummaries"
        :max-height="maxHeight-220"
      >
        <el-table-column label="序号" type="index" align="center" width="60">
          <template #default="{ row: { sourceRow: row },$index }">
            <span v-if="row.boolSurplus">余料</span>
            <span v-else>{{$index+1}}</span>
          </template>
        </el-table-column>
        <el-table-column key="serialNumber" prop="serialNumber" label="物料编号" align="center" show-overflow-tooltip />
        <el-table-column key="classifyName" prop="classifyName" label="物料种类" align="center" show-overflow-tooltip />
        <el-table-column key="length" prop="length" label="长(mm)" align="center" show-overflow-tooltip />
        <el-table-column key="width" prop="width" label="宽(mm)" align="center" show-overflow-tooltip />
        <el-table-column key="thickness" prop="thickness" label="厚(mm)" align="center" show-overflow-tooltip />
        <el-table-column key="specification" prop="specification" label="规格" align="center" show-overflow-tooltip />
        <el-table-column key="quantity" prop="quantity" label="数量（张）" align="center" show-overflow-tooltip />
        <el-table-column key="mete" prop="mete" label="重量（kg）" align="center" show-overflow-tooltip />
        <el-table-column key="project" prop="project" label="所属项目" align="center" min-width="150" show-overflow-tooltip />
        <el-table-column key="monomerName" prop="monomerName" label="单体" align="center" show-overflow-tooltip />
        <el-table-column key="areaName" prop="areaName" label="区域" align="center" show-overflow-tooltip />
        <el-table-column key="workshopName" prop="workshopName" label="车间" align="center" show-overflow-tooltip />
        <el-table-column key="warehouse.name" prop="warehouse.name" label="仓库" align="center" show-overflow-tooltip />
        <el-table-column key="recipientName" prop="recipientName" label="领用人" align="center" show-overflow-tooltip />
        <el-table-column key="outboundTime" prop="outboundTime" label="出库时间" align="center" show-overflow-tooltip width="150" />
        <el-table-column key="boolOutbound" prop="boolOutbound" label="是否出库" align="center" show-overflow-tooltip />
      </common-table>
    </template>
  </common-drawer>
</template>

<script setup>
import { getDetail, reviewPassed, reviewReturned } from '@/api/wms/report/raw-material/convert-list'
import { ref, defineProps, defineEmits, watch } from 'vue'
import { ElMessage } from 'element-plus'

import ReviewConfirmButton from '@/components-system/common/review-confirm-button.vue'
import { whetherEnum } from '@enum-ms/common'
import { projectNameFormatter } from '@/utils/project'

import { setSpecInfoToList } from '@/utils/wms/spec'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { tableSummary } from '@/utils/el-extra'
import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'

const emit = defineEmits(['success', 'update:modelValue', 'refresh'])
const { visible, handleClose } = useVisible({ emit, props })

const props = defineProps({
  detailInfo: {
    type: Object,
    default: () => {}
  },
  permission: {
    type: Object,
    default: () => {}
  }
})

const loading = ref(false)
const detail = ref({})
const passedLoading = ref(false)
const returnedLoading = ref(false)
const detailRef = ref()

// const steelPlateType = {
//   PLATE: { L: '整料', K: 'PLATE', V: false },
//   SURPLUS: { L: '余料', K: 'SURPLUS', V: true }
// }

const dataFormat = ref([
  ['project', 'parse-project'],
  ['quantity', ['to-fixed-field', 'measurePrecision']],
  ['mete', ['to-fixed-field', 'accountingPrecision']],
  ['outboundTime', ['parse-time', '{y}-{m}-{d} {h}:{i}']],
  ['boolOutbound', ['parse-enum', whetherEnum]]
])

const { maxHeight } = useMaxHeight(
  {
    mainBox: '.convert-detail',
    extraBox: '.el-drawer__header',
    wrapperBox: '.el-drawer__body',
    paginate: true,
    minHeight: 300,
    navbar: false,
    clientHRepMainH: true
  },
  detailRef
)

// 合计
function getSummaries(param) {
  const summary = tableSummary(param, {
    props: ['quantity', 'mete']
  })
  return summary
}

watch(
  () => props.detailInfo,
  (val) => {
    if (val.id) {
      fetchDetail()
    }
  },
  { deep: true, immediate: true }
)

async function fetchDetail() {
  detail.value = {}
  loading.value = true
  try {
    const data = await getDetail(props.detailInfo.id)
    detail.value = data || {}
    detail.value.widthThick = (detail.value.thickness || detail.value.width) ? (detail.value.thickness || '-') + '*' + (detail.value.width || '-') : '-'
    await setSpecInfoToList(detail.value.convertDetailDTOList)
    detail.value.convertDetailDTOList = await numFmtByBasicClass(
      detail.value.convertDetailDTOList,
      {
        toSmallest: false,
        toNum: true
      }
    )
  } catch (error) {
    console.log(error)
  } finally {
    loading.value = false
  }
}

// 通过提交
async function passed() {
  try {
    passedLoading.value = true
    await reviewPassed(props.detailInfo.id)
    ElMessage.success('通过成功')
    emit('refresh')
    handleClose()
  } catch (error) {
    console.log('通过提交', error)
  } finally {
    passedLoading.value = false
  }
}

// 退回
async function returned() {
  try {
    returnedLoading.value = true
    await reviewReturned(props.detailInfo.id)
    ElMessage.success('退回成功')
    emit('refresh')
    handleClose()
  } catch (error) {
    console.log('退回', error)
  } finally {
    returnedLoading.value = false
  }
}
</script>
<style lang="scss" scoped>
.imgs-box {
  & > .el-image {
    width: 50px;
    height: 40px;
    border: 2px solid #dcdfe6;
    border-radius: 6px;
    background-color: white;
    cursor: pointer;
    + .el-image {
      margin-left: -40px;
    }
  }
}
</style>
