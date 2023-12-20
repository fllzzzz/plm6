<template>
  <common-drawer
    ref="detailRef"
    append-to-body
    :close-on-click-modal="false"
    :before-close="handleClose"
    v-model="visible"
    title="详情"
    :wrapper-closable="false"
    size="80%"
    custom-class="convert-detail"
  >
    <template #titleAfter>
      <el-tag type="success">转换单：{{detailInfo.receiptSerialNumber}}</el-tag>
      <el-tag type="info">出库单：{{detailInfo.outSerialNumber}}</el-tag>
      <el-tag type="warning">出库审核时间：{{detailInfo.createTime || '-'}}</el-tag>
    </template>
    <template #content>
      <el-tag>原物料信息</el-tag>
      <el-descriptions :column="3" border style="margin:10px 0;">
        <el-descriptions-item label-class-name="contractLabel" label="物料名称">{{ detailInfo.classifyName }}</el-descriptions-item>
        <el-descriptions-item label-class-name="contractLabel" label="物料编号">{{ detailInfo.serialNumber }}</el-descriptions-item>
        <el-descriptions-item label-class-name="contractLabel" label="规格">
          {{ detailInfo.specification }}
        </el-descriptions-item>
        <el-descriptions-item label-class-name="contractLabel" label="厚*宽">
          {{ detailInfo.widthThick}}
        </el-descriptions-item>
        <el-descriptions-item label-class-name="contractLabel" label="品牌">
          {{ detailInfo.brand }}
        </el-descriptions-item>
        <el-descriptions-item label-class-name="contractLabel" label="颜色">
          {{ detailInfo.color }}
        </el-descriptions-item>
        <el-descriptions-item label-class-name="contractLabel" label="卷号">
          {{ detailInfo.heatNoAndBatchNo }}
        </el-descriptions-item>
        <el-descriptions-item label-class-name="contractLabel" label="项目">
          {{ detailInfo.project }}
        </el-descriptions-item>
        <el-descriptions-item label-class-name="contractLabel" label="仓库">
          {{ detailInfo.warehouse?.name }}
        </el-descriptions-item>
        <el-descriptions-item label-class-name="contractLabel" label="备注">
          {{ detailInfo.remark }}
        </el-descriptions-item>
      </el-descriptions>
      <el-divider><span class="title">转换后物料信息</span></el-divider>
      <common-table
        :data="list"
        v-loading="loading"
        :data-format="dataFormat"
        show-summary
        :summary-method="getSummaries"
        :max-height="maxHeight-220"
      >
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column key="serialNumber" prop="serialNumber" label="物料编号" align="center" show-overflow-tooltip />
        <el-table-column key="classifyName" prop="classifyName" label="物料名称" align="center" show-overflow-tooltip />
        <el-table-column key="specification" prop="specification" label="规格" align="center" show-overflow-tooltip />
        <el-table-column key="thickness" prop="thickness" label="厚(mm)" align="center" show-overflow-tooltip />
        <el-table-column key="width" prop="width" label="宽(mm)" align="center" show-overflow-tooltip />
        <el-table-column key="length" prop="length" label="长(mm)" align="center" show-overflow-tooltip />
        <el-table-column key="brand" prop="brand" label="品牌" align="center" show-overflow-tooltip />
        <el-table-column key="heatNoAndBatchN" prop="heatNoAndBatchN" label="炉批号" align="center" show-overflow-tooltip />
        <el-table-column key="quantity" prop="quantity" label="数量（张）" align="center" show-overflow-tooltip />
        <el-table-column key="mete" prop="mete" label="重量（kg）" align="center" show-overflow-tooltip />
        <el-table-column key="project" prop="project" label="项目" align="center" show-overflow-tooltip />
        <el-table-column key="monomer.name" prop="monomer.name" label="单体" align="center" show-overflow-tooltip />
        <el-table-column key="area.name" prop="area.name" label="区域" align="center" show-overflow-tooltip />
        <el-table-column key="createTime" prop="createTime" label="出库时间" align="center" show-overflow-tooltip />
        <el-table-column key="warehouse.name" prop="warehouse.name" label="仓库" align="center" show-overflow-tooltip />
        <el-table-column key="recipientName" prop="recipientName" label="领用人" align="center" show-overflow-tooltip />
      </common-table>
    </template>
  </common-drawer>
</template>

<script setup>
import { getDetail } from '@/api/wms/report/raw-material/convert-list'
import { ref, defineProps, defineEmits, watch } from 'vue'

import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { tableSummary } from '@/utils/el-extra'
import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'

const emit = defineEmits(['success', 'update:modelValue'])
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
const detailRef = ref()
const list = ref([])

const dataFormat = ref([
  ['project', 'parse-project'],
  ['quantity', ['to-fixed-field', 'measurePrecision']],
  ['mete', ['to-fixed-field', 'accountingPrecision']]
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
  list.value = []
  loading.value = true
  try {
    const { data } = await getDetail(props.detailInfo.id)
    list.value = data || []
    list.value = await numFmtByBasicClass(
      list.value,
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
