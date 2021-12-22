<template>
  <common-drawer ref="drawerRef" title="未完成清单" v-model="drawerVisible" direction="rtl" :before-close="handleClose" size="80%">
    <template #content>
      <common-table :data="list" v-loading="tableLoading" :max-height="maxHeight" style="width: 100%">
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column prop="productionLine.name" :show-overflow-tooltip="true" label="生产线" min-width="140px">
          <template v-slot="scope">
            <span>{{ emptyTextFormatter(scope.row.workshop.name) }}>{{ emptyTextFormatter(scope.row.productionLine.name) }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="project.shortName" label="所属项目" min-width="200px">
          <template v-slot="scope">
            <span class="project-name">{{ projectNameFormatter(scope.row.project) }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="monomer.name" :show-overflow-tooltip="true" label="单体区域" min-width="140px">
          <template v-slot="scope">
            <span>{{ emptyTextFormatter(scope.row.monomer.name) }}>{{ emptyTextFormatter(scope.row.areaDetail.name) }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="serialNumber" :show-overflow-tooltip="true" label="编号" min-width="100px">
          <template v-slot="scope">
            <span>{{ emptyTextFormatter(scope.row.serialNumber) }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="specification" :show-overflow-tooltip="true" label="规格" min-width="110px">
          <template v-slot="scope">
            <span>{{ emptyTextFormatter(scope.row.specification) }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="taskQuantity" label="任务数" min-width="100px" align="center">
          <template v-slot="scope">
            <span>{{ emptyTextFormatter(scope.row.taskQuantity) }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="askCompleteTime" label="要求完成日期" align="center" min-width="110px">
          <template v-slot="scope">
            <span v-parse-time="'{y}-{m}-{d}'">{{ scope.row.askCompleteTime }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="unCompleteQuantity" label="未完成数" min-width="100px" align="center">
          <template v-slot="scope">
            <span class="tc-danger">{{ emptyTextFormatter(scope.row.unCompleteQuantity) }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="unCompleteMete" label="未完成量" min-width="100px" align="center">
          <template v-slot="scope">
            <span class="tc-danger">{{ emptyTextFormatter(scope.row.unCompleteMete) }}</span>
          </template>
        </el-table-column>
      </common-table>
    </template>
  </common-drawer>
</template>

<script setup>
import { getDetail as detail } from '@/api/mes/production-manage/analysis/delay-report'
import { defineProps, defineEmits, ref, watch, inject } from 'vue'

import { emptyTextFormatter } from '@/utils/data-type'
import { projectNameFormatter } from '@/utils/project'
import { deepClone } from '@data-type/index'
import { DP } from '@/settings/config'
import { toFixed } from '@data-type/index'
import { convertUnits } from '@/utils/convert/unit'
import { componentTypeEnum } from '@enum-ms/mes'

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

async function fetchList() {
  try {
    tableLoading.value = true
    const _query = Object.assign(deepClone(query), { dateTime: props.info.askCompleteTime })
    const data = await detail(_query)
    if (query.componentType === componentTypeEnum.ARTIFACT.V) {
      list.value = data.artifactAssembleList.map((v) => {
        v.unCompleteQuantity = v.taskQuantity - v.completeQuantity
        v.unCompleteMete = v.taskNetWeight ? toFixed(v.taskNetWeight - v.completeNetWeight, DP.COM_WT__KG) : '/'
        return v
      })
    } else {
      list.value = data.enclosureList.map((v) => {
        v.unCompleteQuantity = v.taskQuantity - v.completeQuantity
        v.unCompleteMete = v.taskLength ? convertUnits(v.taskLength - v.completeLength, 'mm', 'm', DP.MES_ENCLOSURE_L__M) : '/'
        return v
      })
    }
  } catch (error) {
    console.log('获取未完成清单', error)
  } finally {
    tableLoading.value = false
  }
}
</script>
