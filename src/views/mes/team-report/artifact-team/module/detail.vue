<template>
  <common-drawer
    ref="drawerRef"
    :title="`生产线：${info.workshop?.name}>${processTypeEnum.VL[info.processType]}>${info.productionLine?.name}`"
    v-model="drawerVisible"
    direction="rtl"
    :before-close="handleClose"
    size="100%"
  >
    <template #titleRight> </template>
    <template #content>
      <common-table v-loading="tableLoading" :data="list" :max-height="maxHeight" style="width: 100%">
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column key="project.shortName" prop="project.shortName" :show-overflow-tooltip="true" label="所属项目" min-width="200">
          <template v-slot="scope">
            <span class="project-name">{{ projectNameFormatter(scope.row.project) }}</span>
          </template>
        </el-table-column>
        <el-table-column key="serialNumber" prop="serialNumber" :show-overflow-tooltip="true" label="编号" min-width="140px">
          <template v-slot="scope">
            <span>{{ scope.row.serialNumber }}</span>
          </template>
        </el-table-column>
        <el-table-column key="specification" prop="specification" :show-overflow-tooltip="true" label="规格" min-width="140px">
          <template v-slot="scope">
            <span>{{ scope.row.specification }}</span>
          </template>
        </el-table-column>
        <el-table-column key="material" prop="material" :show-overflow-tooltip="true" label="材质" min-width="80px">
          <template v-slot="scope">
            <span>{{ scope.row.material }}</span>
          </template>
        </el-table-column>
        <el-table-column
          key="netWeight"
          prop="netWeight"
          :show-overflow-tooltip="true"
          :label="`单净重\n(kg)`"
          align="center"
          min-width="80px"
        >
          <template v-slot="scope">
            {{ toFixed(scope.row.netWeight, DP.COM_WT__KG) }}
          </template>
        </el-table-column>
        <el-table-column key="taskQuantity" prop="taskQuantity" :show-overflow-tooltip="true" label="任务总数" align="center" width="100px">
          <template v-slot="scope">
            <span>{{ scope.row.taskQuantity }}</span>
          </template>
        </el-table-column>
        <template v-for="item in processList" :key="item.id">
          <el-table-column
            :label="item.name"
            align="center"
            width="100px"
          >
            <template v-slot="scope">
              <span>{{ scope.row.processMap&&scope.row.processMap[item.id]?.completeQuantity }}</span>
            </template>
          </el-table-column>
        </template>
      </common-table>
    </template>
  </common-drawer>
</template>

<script setup>
import { detail } from '@/api/mes/team-report/artifact-team'
import { defineProps, defineEmits, ref, watch } from 'vue'

import { processTypeEnum } from '@enum-ms/mes'
import { projectNameFormatter } from '@/utils/project'
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

const tableLoading = ref(false)
const list = ref([])
const processList = ref([])
async function fetchList() {
  try {
    tableLoading.value = true
    const { content, process } = await detail(props.info.id)
    list.value = content.map((v) => {
      const _processMap = {}
      v.process.forEach((v) => {
        _processMap[v.id] = v
      })
      v.processMap = _processMap
      return v
    })
    processList.value = process
  } catch (error) {
    console.log('获取结构班组详情', error)
  } finally {
    tableLoading.value = false
  }
}
</script>
