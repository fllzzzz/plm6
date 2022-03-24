<template>
  <common-drawer
    ref="drawerRef"
    :title="`生产线：${info.workshop?.name}>${componentTypeEnum.V[info.productType]?.SL}>${info.productionLine?.name}`"
    v-model="drawerVisible"
    direction="rtl"
    :before-close="handleClose"
    size="100%"
  >
    <template #titleRight> </template>
    <template #content>
      <common-table v-loading="tableLoading" :data="list" :max-height="maxHeight" row-key="rowId" style="width: 100%">
        <el-table-column label="序号" type="index" align="center" width="60" />
        <belonging-info-columns showProject showMonomer showArea fixedWidth />
        <productType-base-info-columns :productType="info?.productType" :unShowField="['material']" />
        <el-table-column
          prop="mete"
          :show-overflow-tooltip="true"
          :label="`${unitObj.label}(${unitObj.unit})`"
          align="center"
          width="100px"
        >
          <template v-slot="scope">
            {{ scope.row.mete }}
          </template>
        </el-table-column>
        <el-table-column key="taskQuantity" prop="taskQuantity" :show-overflow-tooltip="true" label="任务总数" align="center" width="100px">
          <template v-slot="scope">
            <span>{{ scope.row.taskQuantity }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="processSequence" :show-overflow-tooltip="true" label="【工序 │ 完成数】" min-width="400px">
          <template v-slot="scope">
            <span v-html="scope.row.processSequence" />
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
import { detail } from '@/api/mes/team-report/artifact-team'
import { defineProps, defineEmits, ref, watch, inject, computed } from 'vue'

import { componentTypeEnum } from '@enum-ms/mes'
import { deepClone } from '@data-type/index'

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
  }
})

const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible' })
const { handleSizeChange, handleCurrentChange, total, setTotalPage, queryPage } = usePagination({ fetchHook: fetchList })

// 高度
const { maxHeight } = useMaxHeight(
  {
    extraBox: ['.el-drawer__header'],
    wrapperBox: ['.el-drawer__body'],
    navbar: false,
    clientHRepMainH: true,
    extraHeight: 60
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

const productType = computed(() => props.info.productType)
const unitObj = computed(() => {
  return useProductSummaryMeteUnit({ productType: productType.value, l_unit: 'mm', w_unit: 'kg', isSingle: true })
})

async function fetchList() {
  try {
    tableLoading.value = true
    const _query = Object.assign(deepClone(query), {
      factoryId: props.info.factory?.id,
      productType: productType.value,
      productionLineId: props.info.productionLine?.id,
      ...queryPage
    })
    const { content, totalElements } = await detail(_query)
    setTotalPage(totalElements)
    list.value = content.map((v, i) => {
      v.rowId = i + '' + Math.random()
      v.processSequence = v.processSummaryDetailsDOList
        .map((o) => {
          return `<span>【 ${o.name} │ <span style="color: #67C23A;">${
            o.completeQuantity === v.taskQuantity ? '√' : o.completeQuantity || 0
          }</span> 】</span>`
        })
        .join('<span>→</span>')
      v.mete = useProductMeteConvert({
        productType: productType.value,
        weight: { num: v.netWeight },
        length: { num: v.length, to: unitObj.value.unit, dp: unitObj.value.dp }
      })
      return v
    })
  } catch (error) {
    console.log('获取结构班组进度详情', error)
  } finally {
    tableLoading.value = false
  }
}
</script>
