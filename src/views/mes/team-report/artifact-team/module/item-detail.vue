<template>
  <common-drawer
    ref="drawerRef"
    :title="`生产线：${info.workshop?.name}>${artifactProcessEnum.VL[info.productType]}>${info.productionLine?.name}>${itemInfo.name}`"
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
        <el-table-column key="taskQuantity" prop="taskQuantity" :show-overflow-tooltip="true" label="排产任务" align="center" min-width="100px">
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
        <el-table-column key="completeMete" prop="completeMete" :show-overflow-tooltip="true" label="完成总量" align="center" min-width="100px">
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
import { defineProps, defineEmits, ref, watch, inject } from 'vue'

import { artifactProcessEnum } from '@enum-ms/mes'
import { projectNameFormatter } from '@/utils/project'
import { deepClone } from '@data-type/index'
import { DP } from '@/settings/config'
import { toFixed } from '@data-type/index'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'

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
  () => [props.visible, props.itemInfo],
  ([visible]) => {
    if (visible) {
      fetchList()
    }
  },
  { immediate: true, deep: true }
)

function getSummaries(param) {
  const { columns, data } = param
  const sums = []
  columns.forEach((column, index) => {
    if (index === 0) {
      sums[index] = '合计'
      return
    }
    if (['taskQuantity', 'completeQuantity', 'unCompleteQuantity', 'completeMete'].includes(column.property)) {
      const values = data.map((item) => Number(item[column.property]))
      if (!values.every((value) => isNaN(value))) {
        sums[index] = values.reduce((prev, curr) => {
          const value = Number(curr)
          if (!isNaN(value)) {
            return prev + curr
          } else {
            return prev
          }
        }, 0)
      }
    }
  })
  return sums
}

const query = inject('query')
const tableLoading = ref(false)
const list = ref([])
const dataPath = {
  [artifactProcessEnum.ONCE.V]: 'assembleList',
  [artifactProcessEnum.TWICE.V]: 'artifactList'
}
async function fetchList() {
  try {
    tableLoading.value = true
    const _productType = props.info.productType
    const _query = Object.assign(deepClone(query), {
      factoryId: props.info.factory?.id,
      processId: props.itemInfo.id,
      productType: _productType,
      productionLineId: props.info.productionLine?.id
    })
    const _data = await detail(_query)
    list.value = _data[dataPath[_productType]].map((v) => {
      v.unCompleteQuantity = v.taskQuantity - v.completeQuantity
      v.completeMete = toFixed(v.completeNetWeight, DP.COM_WT__KG)
      return v
    })
  } catch (error) {
    console.log('获取结构班组工序详情', error)
  } finally {
    tableLoading.value = false
  }
}
</script>
