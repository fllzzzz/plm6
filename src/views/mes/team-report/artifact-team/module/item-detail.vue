<template>
  <common-drawer
    ref="drawerRef"
    :title="`生产线：${info.workshop?.name}>${componentTypeEnum.V[info.productType]?.SL}>${info.productionLine?.name}>${itemInfo.name}`"
    v-model="drawerVisible"
    direction="rtl"
    :before-close="handleClose"
    size="70%"
  >
    <template #titleRight>
      <div class="print-wrap">
        <print-table
          v-permission="permission.printDetail"
          api-key="mesStructureProcess"
          :params="printParams"
          size="mini"
          type="warning"
          class="filter-item"
        />
      </div>
    </template>
    <template #content>
      <common-table v-loading="tableLoading" :data="list" row-key="rowId" :max-height="maxHeight" style="width: 100%">
        <el-table-column label="序号" type="index" align="center" width="60" />
        <belonging-info-columns showProject showMonomer showArea />
        <productType-base-info-columns :productType="info?.productType" :unShowField="['material']" />
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
      <!--分页组件-->
      <el-pagination
        :total="total"
        :current-page="queryPage.pageNumber"
        :page-size="queryPage.pageSize"
        style="margin-top: 8px"
        layout="total, prev, pager, next, sizes"
        @size-change="handleSizeChange"
        @current-change="handleCurrentChange"
      />
    </template>
  </common-drawer>
</template>

<script setup>
import { processDetail as detail } from '@/api/mes/team-report/artifact-team'
import { defineProps, defineEmits, ref, watch, inject, computed } from 'vue'

import { componentTypeEnum } from '@enum-ms/mes'
import { deepClone } from '@data-type/index'
// import { tableSummary } from '@/utils/el-extra'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import usePagination from '@compos/use-pagination'
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
  },
  itemInfo: {
    type: Object,
    default: () => {}
  }
})

const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible' })
const { handleSizeChange, handleCurrentChange, total, setTotalPage, queryPage } = usePagination({ fetchHook: fetchList })

const permission = inject('permission')

// 高度
const { maxHeight } = useMaxHeight(
  {
    extraBox: ['.el-drawer__header'],
    wrapperBox: ['.el-drawer__body'],
    navbar: false,
    paginate: true,
    clientHRepMainH: true,
    extraHeight: 60
  },
  drawerRef
)

watch(
  () => [props.visible, props.itemInfo],
  ([visible]) => {
    if (visible) {
      fetchList()
    }
  },
  { immediate: true, deep: true }
)

// function getSummaries(param) {
//   return tableSummary(param, { props: ['taskQuantity', 'completeQuantity', ['taskMete', unitObj.value.DP], ['completeMete', unitObj.value.DP]] })
// }
const query = inject('query')
const tableLoading = ref(false)
const list = ref([])
const unitObj = computed(() => {
  return useProductSummaryMeteUnit({ productType: props.info.productType, w_unit: 'kg' })
})

const printParams = computed(() => {
  return Object.assign(deepClone(query), {
    factoryId: props.info.factory?.id,
    processId: props.itemInfo.id,
    productType: props.info.productType,
    productionLineId: props.info.productionLine?.id
  })
})

async function fetchList() {
  try {
    tableLoading.value = true
    const { content, totalElements } = await detail({ ...printParams.value, ...queryPage })
    setTotalPage(totalElements)
    list.value = content.map((v, i) => {
      v.rowId = i + '' + Math.random()
      // v.unCompleteQuantity = v.taskQuantity - v.completeQuantity
      v.completeMete = useProductMeteConvert({
        productType: v.productType,
        weight: { num: v.completeNetWeight },
        length: { num: v.completeLength, to: unitObj.value.unit, dp: unitObj.value.dp }
      })
      v.taskMete = useProductMeteConvert({
        productType: v.productType,
        weight: { num: v.taskNetWeight },
        length: { num: v.taskLength, to: unitObj.value.unit, dp: unitObj.value.dp }
      })
      return v
    })
  } catch (error) {
    console.log('获取结构班组进度工序详情', error)
  } finally {
    tableLoading.value = false
  }
}
</script>
