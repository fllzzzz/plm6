<template>
  <common-drawer
    ref="drawerRef"
    :title="`生产线：${info.workshop?.name}>${info.productionLine?.name}`"
    v-model="drawerVisible"
    direction="rtl"
    :before-close="handleClose"
    size="70%"
  >
    <template #titleRight> </template>
    <template #content>
      <common-table
        v-loading="tableLoading"
        show-summary
        :summary-method="getSummaries"
        :data="list"
        row-key="rowId"
        :max-height="maxHeight"
        style="width: 100%"
      >
        <el-table-column label="序号" type="index" align="center" width="60" />
        <belonging-info-columns showProject showMonomer showArea />
        <productType-base-info-columns :productType="info?.productType" />
        <el-table-column
          prop="taskQuantity"
          :show-overflow-tooltip="true"
          :label="`任务数(${unitObj.measure})`"
          align="center"
          min-width="100px"
        >
          <template #default="{ row }">
            <span>{{ row.taskQuantity }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="taskMete" :show-overflow-tooltip="true" :label="`任务量(${unitObj.unit})`" align="center" min-width="100px">
          <template #default="{ row }">
            <span>{{ row.taskMete }}</span>
          </template>
        </el-table-column>
        <el-table-column
          key="completeQuantity"
          prop="completeQuantity"
          :show-overflow-tooltip="true"
          :label="`完成数(${unitObj.measure})`"
          align="center"
          min-width="100px"
        >
          <template #default="{ row }">
            <span :class="row.completeQuantity === row.taskQuantity ? 'tc-success' : 'tc-danger'">{{ row.completeQuantity }}</span>
          </template>
        </el-table-column>
        <!-- <el-table-column
          key="unCompleteQuantity"
          prop="unCompleteQuantity"
          :show-overflow-tooltip="true"
          label="未完成"
          align="center"
          min-width="100px"
        >
          <template #default="{row}">
            <span class="tc-danger">{{ row.unCompleteQuantity }}</span>
          </template>
        </el-table-column> -->
        <el-table-column
          key="completeMete"
          prop="completeMete"
          :show-overflow-tooltip="true"
          :label="`完成量(${unitObj.unit})`"
          align="center"
          min-width="100px"
        >
          <template #default="{ row }">
            <span :class="row.completeQuantity === row.taskQuantity ? 'tc-success' : 'tc-danger'">{{ row.completeMete }}</span>
          </template>
        </el-table-column>
      </common-table>
    </template>
  </common-drawer>
</template>

<script setup>
import { detail } from '@/api/mes/team-report/enclosure-team'
import { defineProps, defineEmits, ref, watch, inject, computed } from 'vue'

import { deepClone } from '@data-type/index'
import { tableSummary } from '@/utils/el-extra'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import useProductSummaryMeteUnit from '@compos/mes/use-product-summary-mete-unit'
import useProductMeteConvert from '@compos/mes/use-product-mete-convert'
import belongingInfoColumns from '@comp-mes/table-columns/belonging-info-columns'
import productTypeBaseInfoColumns from '@comp-mes/table-columns/productType-base-info-columns'

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
  () => [props.visible, props.info],
  ([visible]) => {
    if (visible) {
      fetchList()
    }
  },
  { immediate: true, deep: true }
)

const query = inject('query')
const tableLoading = ref(false)
const list = ref([])

function getSummaries(param) {
  return tableSummary(param, { props: ['taskQuantity', 'completeQuantity', ['taskMete', unitObj.value.DP], ['completeMete', unitObj.value.DP]] })
}

const unitObj = computed(() => {
  return useProductSummaryMeteUnit({ productType: props.info.productType, w_unit: 'kg' })
})

async function fetchList() {
  try {
    tableLoading.value = true
    const _query = Object.assign(
      {
        factoryId: props.info.factory?.id,
        productType: props.info.productType,
        productionLineId: props.info.productionLine?.id,
        projectId: props.info.project?.id
      },
      deepClone(query)
    )
    const content = await detail(_query)
    list.value = content.map((v, i) => {
      v.rowId = i + '' + Math.random()
      // v.unCompleteQuantity = v.taskQuantity - v.completeQuantity
      v.completeMete = useProductMeteConvert({
        productType: props.info?.productType,
        weight: { num: v.completeNetWeight },
        length: { num: v.completeLength, to: unitObj.value.unit, dp: unitObj.value.dp }
      })
      v.taskMete = useProductMeteConvert({
        productType: props.info?.productType,
        weight: { num: v.taskNetWeight },
        length: { num: v.taskLength, to: unitObj.value.unit, dp: unitObj.value.dp }
      })
      return v
    })
  } catch (error) {
    console.log('获取围护班组进度详情', error)
  } finally {
    tableLoading.value = false
  }
}
</script>
