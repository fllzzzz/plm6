<template>
  <div>
    <!--工具栏-->
    <mHeader ref="headerRef" v-bind="$attrs" @checkSubmit="checkModifyData" :showAble="showAble" :submitList="submitList" :rowIds="rowIds" @showVisible="showVisible"/>
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      return-source-data
      style="width: 100%"
      class="businessTable"
      :max-height="maxHeight"
      :showEmptySymbol="false"
      :cell-class-name="wrongCellMask"
      @selection-change="selectionChange"
    >
      <el-table-column type="selection" align="center" width="60" class="selection" :selectable="selectable" />
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column
        v-if="columns.visible('name')"
        key="name"
        prop="name"
        show-overflow-tooltip
        label="名称"
        align="center"
        min-width="140"
      />
      <el-table-column
        v-if="columns.visible('specification')"
        key="specification"
        prop="specification"
        show-overflow-tooltip
        label="规格"
        align="center"
        min-width="140"
      />
      <el-table-column
        v-if="columns.visible('material')"
        key="material"
        prop="material"
        show-overflow-tooltip
        label="材质"
        align="center"
        min-width="120"
      />
      <el-table-column
        v-if="columns.visible('totalQuantity')"
        key="totalQuantity"
        prop="totalQuantity"
        label="数量"
        align="center"
        min-width="70"
        show-overflow-tooltip
      />
      <el-table-column
        v-if="columns.visible('totalLength')"
        key="totalLength"
        prop="totalLength"
        label="总长度(米)"
        align="center"
        min-width="70"
        show-overflow-tooltip
      />
      <el-table-column
        v-if="columns.visible('totalWeight')"
        key="totalWeight"
        prop="totalWeight"
        show-overflow-tooltip
        label="总量(t)"
        align="center"
        min-width="120"
      />
      <el-table-column
        v-if="columns.visible('pricingManner')"
        key="pricingManner"
        prop="pricingManner"
        show-overflow-tooltip
        label="计价方式"
        align="center"
        min-width="120"
      >
        <template #default="{ row }">
          <common-select
            v-if="headerRef && headerRef.modifying && (crud.selections && crud.selections.findIndex(v=>v.id===row.id)>-1)"
            v-model="row.pricingManner"
            :options="pricingMannerEnum.ENUM"
            default
            type="enum"
            size="small"
            class="filter-item"
          />
          <template v-else>
            <span :class="row.status === 1 ? 'tc-danger' : ''">{{ pricingMannerEnum.VL[row.pricingManner] }}</span>
          </template>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('unitPrice')"
        key="unitPrice"
        prop="unitPrice"
        show-overflow-tooltip
        label="综合单价"
        align="center"
        min-width="120"
      >
        <template #default="{ row }">
          <common-input-number
            v-if="headerRef && headerRef.modifying && (crud.selections && crud.selections.findIndex(v=>v.id===row.id)>-1)"
            v-model="row.unitPrice"
            :step="1"
            :min="0"
            :max="99999999"
            :precision="decimalPrecision.contract===2?3:decimalPrecision.contract"
            :placeholder="crud.selections.findIndex(v=>v.id===row.id) === 0 ? '' : (row.unitPrice || '')"
            size="small"
            style="width: 100%"
            @change="handlePrice(row)"
          />
          <template v-else>
            <span :class="row.status === 1 ? 'tc-danger' : ''">{{ row.unitPrice!=='同上'?toThousand(row.unitPrice,(decimalPrecision.contract===2?3:decimalPrecision.contract)):'-' }}</span>
          </template>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('totalPrice')" key="totalPrice" prop="totalPrice" align="center" min-width="120" label="金额">
        <template #default="{ row }">
          <span :class="row.status === 1 ? 'tc-danger' : ''" v-thousand="{val:row.totalPrice ||0, dp:(decimalPrecision.contract===2?3:decimalPrecision.contract)}" />
        </template>
      </el-table-column>
      <!--详情-->
      <el-table-column v-if="checkPermission([...permission.detail])" label="操作" width="100px" align="center" fixed="right">
        <template #default="{ row }">
          <common-button icon="el-icon-view" type="info" size="mini" @click.stop="openDetail(row)" />
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <mDetail :detail-info="detailInfo" @refresh="crud.refresh" />
  </div>
</template>

<script setup>
import crudApi from '@/api/contract/sales-manage/price-manage/structure'
import { ref, defineExpose, defineEmits } from 'vue'
import { priceManagePM as permission } from '@/page-permission/contract'

import { ElMessage } from 'element-plus'
import { toThousand } from '@/utils/data-type/number'
import { isNotBlank } from '@data-type/index'
import useTableValidate from '@compos/form/use-table-validate'
import checkPermission from '@/utils/system/check-permission'
import { pricingMannerEnum } from '@enum-ms/contract'
import useDecimalPrecision from '@compos/store/use-decimal-precision'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import mDetail from './module/detail'

const { decimalPrecision } = useDecimalPrecision()

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const headerRef = ref()
const detailInfo = ref({})
const showAble = ref(false)
const submitList = ref([])
const rowIds = ref([])
const emit = defineEmits(['showLog'])
const { crud, columns } = useCRUD(
  {
    title: '结构价格',
    sort: [],
    permission: { ...permission },
    crudApi: { ...crudApi },
    optShow: { ...optShow },
    requiredQuery: ['monomerId']
  },
  tableRef
)

function selectable(row) {
  return row.status !== 1
}

const { maxHeight } = useMaxHeight({
  paginate: true,
  extraHeight: 100
})

const validatePrice = (value, row) => {
  return isNotBlank(value)
}

const tableRules = {
  unitPrice: [{ validator: validatePrice, message: '请填写单价', trigger: ['blur', 'change'] }]
}

const ditto = new Map([
  ['pricingManner', -1],
  ['unitPrice', '同上']
])

const { tableValidate, cleanUpData, wrongCellMask } = useTableValidate({ rules: tableRules, ditto })

function selectionChange(val) {
  rowIds.value = []
  crud.selectionChangeHandler(val)
  crud.selections.sort(function (a, b) { return a.orderIndex - b.orderIndex })
  val.forEach(v => {
    rowIds.value.push(v.id)
  })
  console.log(rowIds.value)
}

async function checkModifyData(val) {
  submitList.value = []
  const _list = crud.selections.map((v) => v)
  const { validResult, dealList } = tableValidate(_list)
  showAble.value = false
  if (validResult) {
    cleanUpData(dealList)
    submitList.value = dealList.filter((v) => (v.pricingManner !== v.originPricingManner && v.unitPrice !== '同上') || (v.unitPrice !== v.originUnitPrice && ((typeof v.originUnitPrice === 'number' && v.originUnitPrice > 0) || v.unitPrice > 0)))
    if (submitList.value.length === 0) {
      ElMessage.error('请修改至少一条数据')
      return
    }
    showAble.value = !!submitList.value.length
  }
}

// 查看构件详情
function openDetail(row) {
  detailInfo.value = row
  crud.toDetail(row)
}

function showVisible() {
  emit('showLog')
}

// 价格变动
function handlePrice(v) {
  v.totalPrice = (v.pricingManner === pricingMannerEnum.WEIGHT.V ? v.totalWeight : v.totalLength) * (v.unitPrice && typeof v.unitPrice === 'number' ? v.unitPrice : 0)
}

defineExpose({
  refresh: crud.refresh
})
</script>
