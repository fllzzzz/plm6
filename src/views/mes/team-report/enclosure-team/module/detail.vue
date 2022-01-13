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
        row-key="id"
        :max-height="maxHeight"
        style="width: 100%"
      >
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column key="project.shortName" prop="project.shortName" :show-overflow-tooltip="true" label="所属项目" min-width="200">
          <template v-slot="scope">
            <span class="project-name">{{ projectNameFormatter(scope.row.project) }}</span>
          </template>
        </el-table-column>
        <el-table-column key="name" prop="name" :show-overflow-tooltip="true" label="名称" min-width="120px">
          <template v-slot="scope">
            <span>{{ scope.row.name }}</span>
          </template>
        </el-table-column>
        <el-table-column key="plate" prop="plate" :show-overflow-tooltip="true" label="板型" min-width="120px">
          <template v-slot="scope">
            <span>{{ scope.row.plate }}</span>
          </template>
        </el-table-column>
        <el-table-column key="length" prop="length" :show-overflow-tooltip="true" :label="`单长(mm)`" align="center" min-width="80px">
          <template v-slot="scope">
            <span>{{ toFixed(scope.row.length, DP.MES_ENCLOSURE_L__MM) }}</span>
          </template>
        </el-table-column>
        <el-table-column key="completeQuantity" prop="completeQuantity" label="数量" align="center" min-width="80px" />
        <el-table-column key="completeArea" prop="completeArea" :label="`总面积(㎡)`" align="center" min-width="80px">
          <template v-slot="scope">
            <span>{{ convertUnits(scope.row.completeArea, 'mm²', '㎡', DP.COM_AREA__M2) }}</span>
          </template>
        </el-table-column>
        <el-table-column
          key="completeLength"
          prop="completeLength"
          :show-overflow-tooltip="true"
          :label="`总长度(m)`"
          align="center"
          min-width="80px"
        >
          <template v-slot="scope">
            <span>{{ convertUnits(scope.row.completeLength, 'mm', 'm', DP.MES_ENCLOSURE_L__M) }}</span>
          </template>
        </el-table-column>
        <el-table-column key="completeTime" prop="completeTime" :show-overflow-tooltip="true" label="生产日期" align="center" width="160px">
          <template v-slot="scope">
            <span v-parse-time="{ val: scope.row.completeTime, fmt: '{y}-{m}-{d}' }" />
          </template>
        </el-table-column>
      </common-table>
    </template>
  </common-drawer>
</template>

<script setup>
import { detail } from '@/api/mes/team-report/enclosure-team'
import { defineProps, defineEmits, ref, watch, inject } from 'vue'

import { projectNameFormatter } from '@/utils/project'
import { deepClone } from '@data-type/index'
import { DP } from '@/settings/config'
import { toFixed } from '@data-type/index'
import { convertUnits } from '@/utils/convert/unit'

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
async function fetchList() {
  try {
    tableLoading.value = true
    const _query = Object.assign(deepClone(query), {
      factoryId: props.info.factory?.id,
      productType: props.info.productType,
      productionLineId: props.info.productionLine?.id,
      projectId: props.info.project?.id
    })
    const content = await detail(_query)
    list.value = content
  } catch (error) {
    console.log('获取围护班组详情', error)
  } finally {
    tableLoading.value = false
  }
}

function getSummaries(param) {
  const { columns, data } = param
  const sums = []
  columns.forEach((column, index) => {
    if (index === 0) {
      sums[index] = '合计'
      return
    }
    if (['completeQuantity'].includes(column.property)) {
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
</script>
