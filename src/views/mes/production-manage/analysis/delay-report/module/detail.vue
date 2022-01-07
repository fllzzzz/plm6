<template>
  <common-drawer ref="drawerRef" title="未完成清单" v-model="drawerVisible" direction="rtl" :before-close="handleClose" size="80%">
    <template #content>
      <common-table row-key="rowId" :data="list" v-loading="tableLoading" :max-height="maxHeight" style="width: 100%">
        <el-table-column label="序号" type="index" align="center" width="60" />
        <belonging-info-columns showProject showMonomer showArea showProductionLine />
        <productType-base-info-columns :productType="query.productType" />
        <el-table-column prop="taskQuantity" :label="`任务数(${unitObj.measure})`" min-width="100px" align="center">
          <template v-slot="scope">
            <span>{{ scope.row.taskQuantity}}</span>
          </template>
        </el-table-column>
        <el-table-column prop="askCompleteTime" label="要求完成日期" align="center" min-width="110px">
          <template v-slot="scope">
            <span v-parse-time="'{y}-{m}-{d}'">{{ scope.row.askCompleteTime }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="unCompleteQuantity" :label="`未完成数(${unitObj.measure})`" min-width="100px" align="center">
          <template v-slot="scope">
            <span class="tc-danger">{{ scope.row.unCompleteQuantity}}</span>
          </template>
        </el-table-column>
        <el-table-column prop="unCompleteMete" :label="`未完成量(${unitObj.unit})`" min-width="100px" align="center">
          <template v-slot="scope">
            <span class="tc-danger">{{ scope.row.unCompleteMete}}</span>
          </template>
        </el-table-column>
      </common-table>
    </template>
  </common-drawer>
</template>

<script setup>
import { getDetail as detail } from '@/api/mes/production-manage/analysis/delay-report'
import { defineProps, defineEmits, ref, watch, inject, computed } from 'vue'

import { deepClone } from '@data-type/index'
import { componentTypeEnum } from '@enum-ms/mes'

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
  () => props.visible,
  (visible) => {
    if (visible) {
      fetchList()
    }
  },
  { immediate: true }
)

const query = inject('query')
const tableLoading = ref(false)
const list = ref([])

const dataPath = {
  [componentTypeEnum.ARTIFACT.V]: 'artifactAssembleList',
  [componentTypeEnum.ENCLOSURE.V]: 'enclosureList'
}
const unitObj = computed(() => {
  return useProductSummaryMeteUnit({ productType: query.productType, w_unit: 'kg' })
})

async function fetchList() {
  try {
    tableLoading.value = true
    const _query = Object.assign(deepClone(query), { dateTime: props.info.askCompleteTime })
    const data = await detail(_query)
    list.value = data[dataPath[query.componentType]].map((v, i) => {
      v.rowId = i + '' + Math.random()
      v.unCompleteQuantity = v.taskQuantity - v.completeQuantity
      v.unCompleteMete = useProductMeteConvert({
        productType: query.productType,
        weight: { num: v.taskNetWeight - v.completeNetWeight },
        length: { num: v.taskLength - v.completeLength, to: unitObj.value.unit, dp: unitObj.value.dp }
      })
      return v
    })
  } catch (error) {
    console.log('获取未完成清单', error)
  } finally {
    tableLoading.value = false
  }
}
</script>
