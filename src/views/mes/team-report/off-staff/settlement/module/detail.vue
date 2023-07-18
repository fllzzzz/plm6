<template>
  <common-drawer
    ref="drawerRef"
    :title="`${info.productionLine?.name}>${info.processName}>${info.leaderName}`"
    v-model="drawerVisible"
    direction="rtl"
    :before-close="handleClose"
    size="70%"
  >
    <template #titleAfter>
      <el-tag type="success" effect="plain" size="medium">
        <span>统计日期：</span>
        <span v-parse-time="{ val: query.startDate, fmt: '{y}-{m}-{d}' }"></span> ~
        <span v-parse-time="{ val: query.endDate, fmt: '{y}-{m}-{d}' }"></span>
      </el-tag>
    </template>
    <template #titleRight>
      <div class="print-wrap">
        <print-table
          v-permission="permission.printDetail"
          api-key="mesWageDetail"
          :params="printParams"
          size="mini"
          type="warning"
          class="filter-item"
        />
      </div>
    </template>
    <template #content>
      <common-table
        ref="tableRef"
        v-loading="tableLoading"
        :data="list"
        :data-format="dataFormat"
        :max-height="maxHeight"
        row-key="rowId"
        show-summary
        :summary-method="getSummaries"
        style="width: 100%"
      >
        <el-table-column label="序号" type="index" align="center" width="60" />
        <belonging-info-columns showProject showMonomer />
        <productType-base-info-columns :productType="query.productType" :unShowField="['material', 'color']" />
        <el-table-column prop="completeQuantity" :show-overflow-tooltip="true" label="完成数" align="center">
          <template v-slot="scope">
            <span>{{ scope.row.completeQuantity }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="completeMete" :show-overflow-tooltip="true" :label="`完成量(${unitObj.unit})`" align="center">
          <template v-slot="scope">
            <span>{{ scope.row.completeMete }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="showUnit" :show-overflow-tooltip="true" label="核算单位" align="center">
          <template v-slot="scope">
            <span>{{ scope.row.showUnit }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="checkMete" :show-overflow-tooltip="true" label="核算量" align="center">
          <template v-slot="scope">
            <span>{{ scope.row.checkMete }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="wage" :show-overflow-tooltip="true" label="工序单价(元)" align="center">
          <template v-slot="scope">
            <span>{{ scope.row.wage }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="price" :show-overflow-tooltip="true" label="工资(元)" align="center">
          <template v-slot="scope">
            <span>{{ scope.row.price }}</span>
          </template>
        </el-table-column>
      </common-table>
    </template>
  </common-drawer>
</template>

<script setup>
import { detail } from '@/api/mes/team-report/off-staff-settlement'
import { defineProps, defineEmits, ref, watch, inject, computed } from 'vue'

import { tableSummary } from '@/utils/el-extra'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import useWageQuotaUnit from '@compos/mes/use-wage-quota-unit'
// import useWageQuotaMeteConvert from '@compos/mes/use-wage-quota-mete-convert'
import useProductMeteConvert from '@compos/mes/use-product-mete-convert'
import useProductSummaryMeteUnit from '@compos/mes/use-product-summary-mete-unit'
import belongingInfoColumns from '@comp-mes/table-columns/belonging-info-columns'
import productTypeBaseInfoColumns from '@comp-mes/table-columns/productType-base-info-columns'
import useDecimalPrecision from '@compos/store/use-decimal-precision'

const { decimalPrecision } = useDecimalPrecision()

const drawerRef = ref()
const emit = defineEmits(['update:visible'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  info: {
    type: Object,
    default: () => {}
  }
})

const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible' })

// 高度
const { maxHeight } = useMaxHeight(
  {
    extraBox: ['.el-drawer__header'],
    wrapperBox: ['.el-drawer__body'],
    navbar: false,
    clientHRepMainH: true
  },
  drawerRef
)

watch(
  () => props.visible,
  (visible) => {
    if (visible) {
      fetchList()
    }
  },
  { immediate: true }
)

const dataFormat = ref([
  ['wage', ['to-fixed-ck', 'YUAN']],
  ['price', ['to-fixed-ck', 'YUAN']]
])

const tableLoading = ref(false)
const list = ref([])
const query = inject('query')
const permission = inject('permission')

const unitObj = computed(() => {
  return useProductSummaryMeteUnit({
    productType: query.productType
  })
})

const printParams = computed(() => {
  return Object.assign(
    {
      factoryId: props.info?.factory?.id,
      productionLineId: props.info?.productionLine?.id,
      workshopId: props.info?.workshop?.id,
      teamId: props.info.teamId
    },
    query
  )
})

async function fetchList() {
  let _list = []
  try {
    tableLoading.value = true
    const { content } = await detail(printParams.value)
    _list = content.map((v, i) => {
      v.rowId = i + '' + Math.random()
      v.showUnit = useWageQuotaUnit({ wageQuotaType: v.wageQuotaType }).meteUnit
      // v.checkMete = useWageQuotaMeteConvert({
      //   length: v.mate,
      //   weight: v.mate,
      //   surfaceArea: v.mate,
      //   wageQuotaType: v.wageQuotaType
      // }).convertMete
      v.checkMete = v.mate
      v.completeMete = useProductMeteConvert({
        productType: query.productType,
        weight: { num: v.completeNetWeight, to: unitObj.value.unit, dp: unitObj.value.dp },
        length: { num: v.completeLength, to: unitObj.value.unit, dp: unitObj.value.dp }
      })
      return v
    })
  } catch (error) {
    console.log('获取详情列表失败')
  } finally {
    list.value = _list
    tableLoading.value = false
  }
}

function getSummaries(param) {
  return tableSummary(param, { props: ['completeQuantity', 'completeMete', ['price', decimalPrecision.value.mes]] })
}
</script>
