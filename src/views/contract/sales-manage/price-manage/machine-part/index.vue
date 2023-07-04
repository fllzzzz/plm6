<template>
  <div>
    <!--工具栏-->
    <mHeader ref="headerRef" v-bind="$attrs" @checkSubmit="checkModifyData" :showAble="showAble"/>
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      return-source-data
      :data-format="dataFormat"
      style="width: 100%"
      class="businessTable"
      :max-height="maxHeight"
      :cell-class-name="wrongCellMask"
    >
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column v-if="columns.visible('name')" key="name" prop="name" show-overflow-tooltip label="名称" align="center" min-width="140" />
      <el-table-column v-if="columns.visible('specification')" key="specification" prop="specification" show-overflow-tooltip label="规格" align="center" min-width="140" />
      <el-table-column v-if="columns.visible('material')" key="material" prop="material" show-overflow-tooltip label="材质" align="center" min-width="120" />
      <el-table-column v-if="columns.visible('totalQuantity')" key="totalQuantity" prop="totalQuantity" label="数量" align="center" min-width="70" show-overflow-tooltip />
      <el-table-column v-if="columns.visible('totalWeight')" key="totalWeight" prop="totalWeight" show-overflow-tooltip label="总量(t)" align="center" min-width="120" />
      <el-table-column v-if="columns.visible('totalLength')" key="totalLength" prop="totalLength" label="总长度(米)" align="center" min-width="70" show-overflow-tooltip />
      <el-table-column v-if="columns.visible('pricingManner')" key="pricingManner" prop="pricingManner" show-overflow-tooltip label="计价方式" align="center" min-width="120">
        <template #default="{ row }">
           <common-select
            v-if="headerRef && headerRef.modifying"
            v-model="row.pricingManner"
            :options="pricingMannerEnum.ENUM"
            default
            type="enum"
            size="small"
            class="filter-item"
          />
          <template v-else>
            <span>{{ pricingMannerEnum.VL[row.pricingManner]}}</span>
          </template>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('unitPrice')" key="unitPrice" prop="unitPrice" show-overflow-tooltip label="综合单价" align="center" min-width="120">
        <template #default="{ row }">
          <common-input-number
            v-if="headerRef && headerRef.modifying"
            v-model="row.newUnitPrice"
            :step="1"
            :min="0"
            :max="99999999"
            :precision="decimalPrecision.contract"
            size="small"
            style="width: 100%"
            @change="handlePrice(row)"
          />
          <template v-else>
          <span>{{ row.unitPrice }}</span>
          </template>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('totalPrice')" key="totalPrice" prop="totalPrice" align="center" min-width="120" label="金额">
        <template #default="{ row }">
          <span :class="row.status === 1 ? 'tc-danger' : ''" v-thousand="{val:row.totalPrice ||0, dp:decimalPrecision.contract}" />
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
  </div>
</template>

<script setup>
import crudApi from '@/api/contract/sales-manage/price-manage/machine-part'
import { ref, defineExpose, computed } from 'vue'
import { priceManagePM as permission } from '@/page-permission/contract'
import { pricingMannerEnum } from '@enum-ms/contract'
import { ElMessage } from 'element-plus'

import useDecimalPrecision from '@compos/store/use-decimal-precision'

// import useTableChange from '@compos/form/use-table-change'
import { validate } from '@compos/form/use-table-validate'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'

const { decimalPrecision } = useDecimalPrecision()

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const headerRef = ref()
const showAble = ref(false)
const dataFormat = computed(() => {
  return [
    ['unitPrice', ['to-thousand', decimalPrecision.value.contract]]
  ]
})
const { crud, columns } = useCRUD(
  {
    title: '散发制品价格',
    sort: [],
    permission: { ...permission },
    crudApi: { ...crudApi },
    optShow: { ...optShow },
    requiredQuery: ['monomerId']
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  paginate: true,
  extraHeight: 100
})

const validatePrice = (value, row) => {
  if (row.pricingManner !== row.originPricingManner || row.unitPrice !== row.originUnitPrice) {
    return !!row.newUnitPrice
  }
  return true
}

const tableRules = {
  unitPrice: [{ validator: validatePrice, message: '请填写单价', trigger: 'blur' }]
}

function wrongCellMask({ row, column }) {
  if (!row) return
  const rules = tableRules
  let flag = true
  if (row.verify && Object.keys(row.verify) && Object.keys(row.verify).length > 0) {
    if (row.verify[column.property] === false) {
      flag = validate(column.property, rules[column.property], row)
    }
    if (flag) {
      row.verify[column.property] = true
    }
  }
  let columnMask = ''
  if (column.property === 'unitPrice') {
    if (row.unitPrice !== row.originUnitPrice) {
      columnMask = 'mask-td'
    }
  }
  if (column.property === 'pricingManner') {
    if (row.pricingManner !== row.originPricingManner) {
      columnMask = 'mask-td'
    }
  }
  return flag ? columnMask : 'wrong-td'
}

async function checkModifyData(val) {
  const rules = tableRules
  let flag = true
  crud.data.map(row => {
    row.verify = {}
    for (const rule in rules) {
      row.verify[rule] = validate(rule, rules[rule], row)
      if (!row.verify[rule]) {
        flag = false
      }
    }
  })
  if (!flag) {
    ElMessage.error('请填写表格中标红数据')
    showAble.value = false
    return false
  } else {
    showAble.value = true
    return true
  }
}

// 价格变动
function handlePrice(row) {
  row.unitPrice = row.newUnitPrice
  row.totalPrice = (row.pricingManner === pricingMannerEnum.WEIGHT.V ? row.totalWeight : row.totalLength) * (row.unitPrice || 0)
}

defineExpose({
  refresh: crud.refresh
})
</script>
