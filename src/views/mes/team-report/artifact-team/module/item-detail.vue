<template>
  <common-drawer
    ref="drawerRef"
    :title="`生产线：${info.workshop?.name}>${artifactProcessEnum.VL[info.productType]}>${info.productionLine?.name}>${itemInfo.name}`"
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
      <common-table
        v-loading="tableLoading"
        show-summary
        :summary-method="getSummaries"
        :data="list"
        :max-height="maxHeight"
        style="width: 100%"
      >
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column key="project.shortName" prop="project.shortName" :show-overflow-tooltip="true" label="所属项目" min-width="200">
          <template v-slot="scope">
            <span class="project-name">{{ projectNameFormatter(scope.row.project) }}</span>
          </template>
        </el-table-column>
        <el-table-column
          v-if="info.productType === artifactProcessEnum.TWICE.V"
          key="name"
          prop="name"
          :show-overflow-tooltip="true"
          label="名称"
          min-width="140px"
        >
          <template v-slot="scope">
            <span>{{ scope.row.name }}</span>
          </template>
        </el-table-column>
        <el-table-column key="serialNumber" prop="serialNumber" :show-overflow-tooltip="true" label="编号" min-width="140px">
          <template v-slot="scope">
            <span>{{ scope.row.serialNumber }}</span>
          </template>
        </el-table-column>
        <el-table-column
          v-if="info.productType === artifactProcessEnum.TWICE.V"
          key="specification"
          prop="specification"
          :show-overflow-tooltip="true"
          label="规格"
          min-width="140px"
        >
          <template v-slot="scope">
            <span>{{ scope.row.specification }}</span>
          </template>
        </el-table-column>
        <el-table-column
          v-if="info.productType === artifactProcessEnum.TWICE.V"
          key="material"
          prop="material"
          :show-overflow-tooltip="true"
          label="材质"
          min-width="80px"
        >
          <template v-slot="scope">
            <span>{{ scope.row.material }}</span>
          </template>
        </el-table-column>
        <el-table-column key="taskQuantity" prop="taskQuantity" :show-overflow-tooltip="true" label="工单任务" align="center" min-width="100px">
          <template v-slot="scope">
            <span>{{ scope.row.taskQuantity }}</span>
          </template>
        </el-table-column>
        <el-table-column
          key="completeQuantity"
          prop="completeQuantity"
          :show-overflow-tooltip="true"
          label="完成数量"
          align="center"
          min-width="100px"
        >
          <template v-slot="scope">
            <span class="tc-success">{{ scope.row.completeQuantity }}</span>
          </template>
        </el-table-column>
        <el-table-column
          key="unCompleteQuantity"
          prop="unCompleteQuantity"
          :show-overflow-tooltip="true"
          label="未完成"
          align="center"
          min-width="100px"
        >
          <template v-slot="scope">
            <span class="tc-danger">{{ scope.row.unCompleteQuantity }}</span>
          </template>
        </el-table-column>
        <el-table-column key="completeMete" prop="completeMete" :show-overflow-tooltip="true" :label="`完成总量(${unitObj.unit})`" align="center" min-width="100px">
          <template v-slot="scope">
            <span class="tc-success">{{ scope.row.completeMete }}</span>
          </template>
        </el-table-column>
      </common-table>
    </template>
  </common-drawer>
</template>

<script setup>
import { processDetail as detail } from '@/api/mes/team-report/artifact-team'
import { defineProps, defineEmits, ref, watch, inject, computed } from 'vue'

import { artifactProcessEnum } from '@enum-ms/mes'
import { projectNameFormatter } from '@/utils/project'
import { deepClone } from '@data-type/index'
import { tableSummary } from '@/utils/el-extra'

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
  },
  itemInfo: {
    type: Object,
    default: () => {}
  }
})

const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible' })

const permission = inject('permission')

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
  () => [props.visible, props.itemInfo],
  ([visible]) => {
    if (visible) {
      fetchList()
    }
  },
  { immediate: true, deep: true }
)

function getSummaries(param) {
  return tableSummary(param, { props: ['taskQuantity', 'completeQuantity', 'unCompleteQuantity', ['completeMete', unitObj.value.DP]] })
}
const query = inject('query')
const tableLoading = ref(false)
const list = ref([])
const dataPath = {
  [artifactProcessEnum.ONCE.V]: 'assembleList',
  [artifactProcessEnum.TWICE.V]: 'artifactList'
}
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
    const _productType = props.info.productType
    const _data = await detail(printParams.value)
    list.value = _data[dataPath[_productType]].map((v) => {
      v.unCompleteQuantity = v.taskQuantity - v.completeQuantity
      v.completeMete = useProductMeteConvert({
        productType: v.productType,
        weight: { num: v.completeNetWeight },
        length: { num: v.completeLength, to: unitObj.value.unit, dp: unitObj.value.dp }
      })
      return v
    })
  } catch (error) {
    console.log('获取结构班组工序详情', error)
  } finally {
    tableLoading.value = false
  }
}
</script>
