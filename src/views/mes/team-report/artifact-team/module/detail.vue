<template>
  <common-drawer
    ref="drawerRef"
    :title="`生产线：${info.workshop?.name}>${artifactProcessEnum.VL[info.productType]}>${info.productionLine?.name}`"
    v-model="drawerVisible"
    direction="rtl"
    :before-close="handleClose"
    size="100%"
  >
    <template #titleRight> </template>
    <template #content>
      <common-table v-loading="tableLoading" :data="list" :max-height="maxHeight" style="width: 100%">
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column prop="project.shortName" :show-overflow-tooltip="true" label="所属项目" min-width="140">
          <template v-slot="scope">
            <span class="project-name">{{ projectNameFormatter(scope.row.project) }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="serialNumber" :show-overflow-tooltip="true" label="编号" min-width="100px">
          <template v-slot="scope">
            <span>{{ scope.row.serialNumber }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="specification" :show-overflow-tooltip="true" label="规格" min-width="100px">
          <template v-slot="scope">
            <span>{{ scope.row.specification }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="material" :show-overflow-tooltip="true" label="材质" min-width="80px">
          <template v-slot="scope">
            <span>{{ scope.row.material }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="mete" :show-overflow-tooltip="true" :label="`${unitObj.label}(${unitObj.unit})`" align="center" width="100px">
          <template v-slot="scope">
            {{ scope.row.mete}}
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
    </template>
  </common-drawer>
</template>

<script setup>
import { detail } from '@/api/mes/team-report/artifact-team'
import { defineProps, defineEmits, ref, watch, inject, computed } from 'vue'

import { artifactProcessEnum } from '@enum-ms/mes'
import { projectNameFormatter } from '@/utils/project'
import { deepClone } from '@data-type/index'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import useProductSummaryMeteUnit from '@compos/mes/use-product-summary-mete-unit'
import useProductMeteConvert from '@compos/mes/use-product-mete-convert'

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
      productionLineId: props.info.productionLine?.id
    })
    const content = await detail(_query)
    list.value = content.map((v) => {
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
    console.log('获取结构班组详情', error)
  } finally {
    tableLoading.value = false
  }
}
</script>
