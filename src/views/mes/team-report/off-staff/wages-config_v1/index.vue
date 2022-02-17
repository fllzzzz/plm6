<template>
  <div class="app-container">
    <div class="head-container">
      <mHeader v-model:modifying="modifying" v-model:wageQuotaType="wageQuotaType" />
    </div>
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      show-summary
      :summary-method="getSummaries"
      row-key="id"
      style="width: 100%"
    >
      <el-table-column label="序号" type="index" align="center" width="60" />
      <belonging-info-columns :columns="columns" showMonomer showArea showTeam />
      <el-table-column
        v-if="columns.visible('serialNumber')"
        key="serialNumber"
        prop="serialNumber"
        :show-overflow-tooltip="true"
        label="编号"
        min-width="140px"
      >
        <template v-slot="scope">
          <span>{{ scope.row.serialNumber }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('specification')"
        key="specification"
        prop="specification"
        :show-overflow-tooltip="true"
        label="规格"
        min-width="140px"
      >
        <template v-slot="scope">
          <span>{{ scope.row.specification }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('taskQuantity')"
        key="taskQuantity"
        prop="taskQuantity"
        label="任务数量"
        align="center"
        width="100px"
      />
      <el-table-column
        v-if="columns.visible('taskMete')"
        key="taskMete"
        prop="taskMete"
        :label="`任务量(${unitObj.unit})`"
        align="center"
        width="100px"
      >
        <template #default="{ row }">
          <span>{{ row.taskMete }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('checkMete')"
        key="checkMete"
        prop="checkMete"
        :label="`核算量(${checkUnitObj.UNIT})`"
        align="center"
        width="100px"
      >
        <template #default="{ row }">
          <span>{{ row.checkMete }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="unitPrice" align="center" width="115px" label="单价(元)">
        <template #default="{ row }">
          <el-input-number
            v-if="modifying"
            v-model="row.unitPrice"
            :min="0"
            :max="9999999999"
            :controls="false"
            :step="5"
            :precision="2"
            size="mini"
            placeholder="单价"
            @change="handleUnitPriceChange($event, row)"
          />
          <span v-else v-to-fixed="{ k: 'YUAN', val: row.unitPrice }"></span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('totalAmount')"
        key="totalAmount"
        prop="totalAmount"
        label="总价(元)"
        align="center"
        width="100px"
      >
        <template #default="{ row }">
          <span v-if="modifying">
            {{ row.totalAmount }}
          </span>
          <span v-else v-to-fixed="{ k: 'YUAN', val: row.totalAmount }"></span>
        </template>
      </el-table-column>
    </common-table>
  </div>
</template>

<script setup>
import crudApi from '@/api/mes/team-report/off-staff'
import { ref, provide, computed } from 'vue'

import { offStaffWagesConfigPM as permission } from '@/page-permission/mes'
import { componentTypeEnum } from '@enum-ms/mes'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import useProductMeteConvert from '@compos/mes/use-product-mete-convert'
import useProductSummaryMeteUnit from '@compos/mes/use-product-summary-mete-unit'
import useWageQuotaUnit from '@compos/mes/use-wage-quota-unit'
import useWageQuotaMeteConvert from '@compos/mes/use-wage-quota-mete-convert'
import belongingInfoColumns from '@comp-mes/table-columns/belonging-info-columns'
import mHeader from './module/header'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '编外-工价',
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    requiredQuery: ['processId', 'projectId'],
    queryOnPresenterCreated: false,
    dataPath: '',
    hasPagination: false
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: false })
provide('query', crud.query)

const modifying = ref(false)

const dataPath = {
  [componentTypeEnum.ARTIFACT.V]: 'artifact',
  [componentTypeEnum.ASSEMBLE.V]: 'assemble',
  [componentTypeEnum.MACHINE_PART.V]: 'machinePart',
  [componentTypeEnum.ENCLOSURE.V]: 'enclosure'
}

function handleUnitPriceChange(val, row) {
  row.totalAmount = (row.checkMete * val).toFixed(2)
}

const wageQuotaType = ref()
const checkUnitObj = computed(() => {
  return useWageQuotaUnit({ wageQuotaType: wageQuotaType.value })
})

const unitObj = computed(() => {
  return useProductSummaryMeteUnit({ productType: crud.query.productType })
})
provide('unitObj', unitObj)
provide('checkUnitObj', checkUnitObj)

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data = res.data[dataPath[crud.query.productType]].map((v, i) => {
    v.id = i + '' + Math.random()
    v.unitPrice = v.wage || 0
    v.originUnitPrice = v.unitPrice
    v.taskMete = useProductMeteConvert({
      productType: crud.query.productType,
      length: { num: v.taskLength, to: unitObj.value.unit, dp: unitObj.value.dp },
      weight: { num: v.taskNetWeight, to: unitObj.value.unit, dp: unitObj.value.dp }
    })
    v.checkMete = useWageQuotaMeteConvert({
      length: v.mate,
      weight: v.mate,
      surfaceArea: v.mate,
      wageQuotaType: v.wageQuotaType
    }).convertMete
    v.totalAmount = v.price?.toFixed(2)
    return v
  })
}

function getSummaries(param) {
  const { columns, data } = param
  const sums = []
  columns.forEach((column, index) => {
    if (index === 0) {
      sums[index] = '合计'
      return
    }
    if (column.property === 'taskQuantity' || column.property === 'taskMete' || column.property === 'totalAmount') {
      const values = data.map((item) => Number(item[column.property]))
      if (!values.every((value) => isNaN(value))) {
        sums[index] = values.reduce((prev, curr) => {
          const value = Number(curr)
          if (!isNaN(value)) {
            return prev + curr
          } else {
            return prev
          }
        }, 0)
        if (column.property === 'taskMete' || column.property === 'totalAmount') {
          sums[index] = sums[index].toFixed(2)
        }
      }
    }
  })
  return sums
}
</script>
