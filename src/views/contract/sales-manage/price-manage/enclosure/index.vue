<template>
  <div>
    <!--工具栏-->
    <mHeader ref="headerRef" v-bind="$attrs" />
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
      :cell-class-name="changedCellMask"
    >
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
      />
      <el-table-column
        v-if="columns.visible('unfoldedWidth') && props.category === TechnologyTypeAllEnum.BENDING.V"
        key="unfoldedWidth"
        prop="unfoldedWidth"
        show-overflow-tooltip
        label="展开宽度(mm)"
        align="center"
      />
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
            v-if="headerRef && headerRef.modifying"
            v-model="row.newUnitPrice"
            :step="1"
            :min="0"
            :max="99999999"
            :precision="DP.YUAN"
            size="small"
            style="width: 100%"
            @change="handlePrice(row)"
          />
          <template v-else>
            <span :class="row.status === 1 ? 'tc-danger' : ''">{{ row.unitPrice }}</span>
          </template>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('totalPrice')" key="totalPrice" prop="totalPrice" align="center" min-width="120" label="金额">
        <template #default="{ row }">
          <span :class="row.status === 1 ? 'tc-danger' : ''" v-thousand="row.totalPrice" />
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
  </div>
</template>

<script setup>
import crudApi from '@/api/contract/sales-manage/price-manage/enclosure'
import { ref, defineExpose, defineProps } from 'vue'

import { priceManagePM as permission } from '@/page-permission/contract'
import { DP } from '@/settings/config'
import { enclosureSettlementTypeEnum, TechnologyTypeAllEnum } from '@enum-ms/contract'

import useTableChange from '@compos/form/use-table-change'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'

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

const sourceMap = new Map([['unitPrice', 'originUnitPrice']])

const tableRef = ref()
const headerRef = ref()
const dataFormat = ref([
  ['thickness', ['to-fixed', DP.MES_ENCLOSURE_T__MM]],
  ['unfoldedWidth', ['to-fixed', DP.MES_ENCLOSURE_T__MM]],
  ['unitPrice', ['to-thousand-ck', 'YUAN']]
])
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
const { changedCellMask } = useTableChange({ fieldMap: sourceMap })

// 价格变动
function handlePrice(row) {
  row.unitPrice = row.newUnitPrice
  row.totalPrice = (row.pricingManner === enclosureSettlementTypeEnum.LENGTH.V ? row.totalLength : row.totalArea) * (row.unitPrice || 0)
}

defineExpose({
  refresh: crud.refresh
})
</script>
