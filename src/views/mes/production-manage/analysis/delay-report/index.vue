<template>
  <div class="app-container">
    <div class="head-container">
      <mHeader />
    </div>
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      style="width: 100%"
    >
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column
        v-if="columns.visible('askCompleteTime')"
        key="askCompleteTime"
        prop="askCompleteTime"
        label="完成节点"
        align="center"
        min-width="70px"
      >
        <template v-slot="scope">
          <span v-parse-time="'{y}-{m}-{d}'">{{ scope.row.askCompleteTime }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('taskQuantity')"
        key="taskQuantity"
        prop="taskQuantity"
        label="计划量"
        align="center"
        min-width="100px"
      >
        <template v-slot="scope">
          <span>{{ scope.row.taskQuantity }} {{unitObj.measure}} | {{ scope.row.taskMete }} {{unitObj.unit}}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('completeQuantity')"
        key="completeQuantity"
        prop="completeQuantity"
        label="完成量"
        align="center"
        min-width="100px"
      >
        <template v-slot="scope">
          <span>{{ scope.row.completeQuantity }} {{unitObj.measure}} | {{ scope.row.completeMete }} {{unitObj.unit}}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('diffQuantity')"
        key="diffQuantity"
        prop="diffQuantity"
        label="差异"
        align="center"
        min-width="100px"
      >
        <template v-slot="scope">
          <span>{{ scope.row.diffQuantity }} {{unitObj.measure}} | {{ scope.row.diffMete }} {{unitObj.unit}}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('completeRate')"
        key="completeRate"
        prop="completeRate"
        label="完成率"
        align="center"
        min-width="70px"
      >
        <template v-slot="scope">
          <span>{{ scope.row.completeRate }}</span>
        </template>
      </el-table-column>
      <el-table-column v-permission="permission.detail" label="操作" width="100px" align="center" fixed="right">
        <template v-slot="scope">
          <common-button size="mini" type="info" @click="toDetail(scope.row)">查看</common-button>
        </template>
      </el-table-column>
    </common-table>
    <mDetail v-model:visible="detailVisible" :info="itemInfo"></mDetail>
  </div>
</template>

<script setup>
import crudApi from '@/api/mes/production-manage/analysis/delay-report'
import { ref, provide, computed } from 'vue'

import { reportComponentTypeEnum } from '@enum-ms/mes'
import { analysisDelayReportPM as permission } from '@/page-permission/mes'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import useProductSummaryMeteUnit from '@compos/mes/use-product-summary-mete-unit'
import useProductMeteConvert from '@compos/mes/use-product-mete-convert'
import mHeader from './module/header'
import mDetail from './module/detail'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '迟滞报表',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    dataPath: '',
    hasPagination: false
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: false })

provide('query', crud.query)
const detailVisible = ref(false)
const itemInfo = ref({})

function toDetail(row) {
  itemInfo.value = row
  detailVisible.value = true
}

const dataPath = {
  [reportComponentTypeEnum.ARTIFACT.V]: 'artifactAssembleList',
  [reportComponentTypeEnum.ENCLOSURE.V]: 'enclosureList'
}
const unitObj = computed(() => {
  return useProductSummaryMeteUnit({ productType: crud.query.productType, w_unit: 'kg' })
})

CRUD.HOOK.handleRefresh = (crud, res) => {
  const productType = crud.query.productType
  res.data = res.data[dataPath[crud.query.componentType]].map((v) => {
    v.taskMete = useProductMeteConvert({
      productType: productType,
      weight: { num: v.taskNetWeight },
      length: { num: v.taskLength, to: unitObj.value.unit, dp: unitObj.value.dp }
    })
    v.completeMete = useProductMeteConvert({
      productType: productType,
      weight: { num: v.completeNetWeight },
      length: { num: v.completeLength, to: unitObj.value.unit, dp: unitObj.value.dp }
    })
    v.diffQuantity = v.taskQuantity - v.completeQuantity || 0
    v.diffMete = (v.taskMete - v.completeMete).toFixed(unitObj.value.DP)
    v.completeRate = Number(v.taskMete) ? ((Number(v.completeMete) / Number(v.taskMete)) * 100).toFixed(2) + '%' : '0%'
    return v
  })
}
</script>
