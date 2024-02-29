<template>
  <div>
    <!--工具栏-->
    <mHeader ref="headerRef" v-bind="$attrs" @checkSubmit="checkModifyData" :showAble="showAble" :rowIds="rowIds" :submitList="submitList" @showVisible="showVisible" />
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
        min-width="120"
      />
      <el-table-column
        v-if="columns.visible('plate') && props.category !== TechnologyTypeAllEnum.BENDING.V"
        key="plate"
        prop="plate"
        show-overflow-tooltip
        label="板型"
        align="center"
        width="100"
      />
      <el-table-column
        v-if="columns.visible('thickness')"
        key="thickness"
        prop="thickness"
        show-overflow-tooltip
        label="厚度(mm)"
        align="center"
      >
        <template #default="{ row }">
          <span>{{row.thickness?row.thickness.toFixed(DP.MES_ENCLOSURE_T__MM):''}}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('unfoldedWidth') && props.category === TechnologyTypeAllEnum.BENDING.V"
        key="unfoldedWidth"
        prop="unfoldedWidth"
        show-overflow-tooltip
        label="展开宽度(mm)"
        align="center"
      >
        <template #default="{ row }">
          <span>{{row.unfoldedWidth?row.unfoldedWidth.toFixed(DP.MES_ENCLOSURE_W__MM):''}}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('bendTimes') && props.category === TechnologyTypeAllEnum.BENDING.V"
        key="bendTimes"
        prop="bendTimes"
        show-overflow-tooltip
        label="折弯次数"
        align="center"
      />
      <el-table-column
        v-if="columns.visible('brand')"
        key="brand"
        prop="brand"
        show-overflow-tooltip
        label="品牌"
        align="center"
      />
      <el-table-column
        v-if="columns.visible('color')"
        key="color"
        prop="color"
        show-overflow-tooltip
        label="颜色"
        align="center"
        width="100"
      />
      <el-table-column
        v-if="columns.visible('totalQuantity')"
        key="totalQuantity"
        prop="totalQuantity"
        :show-overflow-tooltip="true"
        label="数量(张)"
        align="center"
        width="100"
      />
      <el-table-column
        v-if="columns.visible('totalArea')"
        key="totalArea"
        prop="totalArea"
        show-overflow-tooltip
        label="总面积(㎡)"
        align="center"
      />
      <el-table-column
        v-if="columns.visible('totalLength')"
        key="totalLength"
        prop="totalLength"
        show-overflow-tooltip
        label="总长度(m)"
        align="center"
      />
      <el-table-column v-if="columns.visible('category')" key="category" prop="category" show-overflow-tooltip label="类别" align="center">
        <template #default="{ row }">
          <span>{{ TechnologyTypeAllEnum.V?.[row.category]?.L }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('pricingManner')"
        key="pricingManner"
        prop="pricingManner"
        show-overflow-tooltip
        label="计价方式"
        align="center"
      >
        <template #default="{ row }">
          <span>{{ enclosureSettlementTypeEnum.VL[row.pricingManner] }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('unitPrice')"
        key="unitPrice"
        prop="unitPrice"
        :show-overflow-tooltip="true"
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
    </common-table>
    <!--分页组件-->
    <pagination />
  </div>
</template>

<script setup>
import crudApi from '@/api/contract/sales-manage/price-manage/enclosure'
import { ref, defineExpose, defineProps, defineEmits } from 'vue'

import { priceManagePM as permission } from '@/page-permission/contract'
import { DP } from '@/settings/config'
import { toThousand } from '@/utils/data-type/number'
import { enclosureSettlementTypeEnum, TechnologyTypeAllEnum } from '@enum-ms/contract'
import useDecimalPrecision from '@compos/store/use-decimal-precision'
import { isNotBlank } from '@data-type/index'
import { ElMessage } from 'element-plus'

import useTableValidate from '@compos/form/use-table-validate'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'

const { decimalPrecision } = useDecimalPrecision()

const props = defineProps({
  category: {
    type: Object
  }
})
const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const emit = defineEmits(['showLog'])

const tableRef = ref()
const headerRef = ref()
const showAble = ref(false)
const submitList = ref([])
const rowIds = ref([])
const { crud, columns } = useCRUD(
  {
    title: '围护价格',
    sort: [],
    permission: { ...permission },
    crudApi: { ...crudApi },
    optShow: { ...optShow },
    requiredQuery: ['enclosurePlanId']
  },
  tableRef
)

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
  ['unitPrice', '同上']
])

function selectable(row) {
  return row.status !== 1
}

const { tableValidate, cleanUpData, wrongCellMask } = useTableValidate({ rules: tableRules, ditto })

function showVisible() {
  emit('showLog')
}

function selectionChange(val) {
  rowIds.value = []
  crud.selectionChangeHandler(val)
  crud.selections.sort(function (a, b) { return a.orderIndex - b.orderIndex })
  val.forEach(v => {
    rowIds.value.push(v.id)
  })
}

async function checkModifyData(val) {
  submitList.value = []
  const _list = crud.selections.map((v) => v)
  const { validResult, dealList } = tableValidate(_list)
  showAble.value = false
  if (validResult) {
    cleanUpData(dealList)
    submitList.value = dealList.filter((v) => (v.unitPrice !== v.originUnitPrice && ((typeof v.originUnitPrice === 'number' && v.originUnitPrice > 0) || v.unitPrice > 0)))
    if (submitList.value.length === 0) {
      ElMessage.error('请修改至少一条数据')
      return
    }
    showAble.value = !!submitList.value.length
  }
}

// 价格变动
function handlePrice(v) {
  v.totalPrice = (v.pricingManner === enclosureSettlementTypeEnum.LENGTH.V ? v.totalLength : v.totalArea) * (v.unitPrice && typeof v.unitPrice === 'number' ? v.unitPrice : 0)
}

defineExpose({
  refresh: crud.refresh
})
</script>
