<template>
  <common-drawer
    ref="drawerRef"
    customClass="group-detail-drawer"
    :title="`车间：${groupDetailData?.workshop?.name}`"
    v-model="drawerVisible"
    direction="rtl"
    :before-close="handleClose"
    size="100%"
  >
    <template #content>
      <div style="display: flex">
        <div style="width: 40%">
          <el-divider class="title"><el-tag type="success" size="medium"> 构件 </el-tag></el-divider>
          <common-table
            ref="directRef"
            highlight-current-row
            :data="artifactGroupList"
            style="width: 100%; cursor: pointer"
            row-key="id"
            :max-height="maxHeight / 3 - 40"
            @row-click="handleRowClick"
          >
            <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
            <el-table-column prop="groups.name" key="groups.name" label="班组" align="center">
              <template #default="{ row }">
                <span>{{ row.groups?.name }}>{{ row.team?.name }}</span>
              </template>
            </el-table-column>
            <el-table-column prop="lastMonthNetWeight" key="lastMonthNetWeight" label="上月产量（吨）" align="center" width="120px">
              <template #default="{ row }">
                <span>{{ (row.lastMonthNetWeight / 1000)?.toFixed(2) }}</span>
              </template>
            </el-table-column>
            <el-table-column prop="yearNetWeight" key="yearNetWeight" label="年度平均产量（吨）" align="center" width="150px">
              <template #default="{ row }">
                <span>{{ (row.yearNetWeight / 1000)?.toFixed(2) }}</span>
              </template>
            </el-table-column>
            <el-table-column prop="unQuantity" key="unQuantity" label="未完成任务（件/吨）" align="center" width="150px">
              <template #default="{ row }">
                <div @click.stop="unComplete(row)">
                  <span class="tc-danger">{{ row.unQuantity }}</span>
                  <span> / </span>
                  <span class="tc-danger">{{ (row.unNetWeight / 1000)?.toFixed(2) }}</span>
                </div>
              </template>
            </el-table-column>
          </common-table>
          <el-divider class="title"><el-tag type="warning" size="medium"> 部件 </el-tag></el-divider>
          <common-table
            ref="indirectRef"
            highlight-current-row
            :data="assembleGroupList"
            style="width: 100%; cursor: pointer"
            row-key="id"
            :max-height="maxHeight / 3 - 40"
            @row-click="handleRowClick"
          >
            <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
            <el-table-column prop="groups.name" key="groups.name" label="班组" align="center">
              <template #default="{ row }">
                <span>{{ row.groups?.name }}>{{ row.team?.name }}</span>
              </template>
            </el-table-column>
            <el-table-column prop="lastMonthNetWeight" key="lastMonthNetWeight" label="上月产量（吨）" align="center" width="120px">
              <template #default="{ row }">
                <span>{{ (row.lastMonthNetWeight / 1000)?.toFixed(2) }}</span>
              </template>
            </el-table-column>
            <el-table-column prop="yearNetWeight" key="yearNetWeight" label="年度平均产量（吨）" align="center" width="150px">
              <template #default="{ row }">
                <span>{{ (row.yearNetWeight / 1000)?.toFixed(2) }}</span>
              </template>
            </el-table-column>
            <el-table-column prop="unQuantity" key="unQuantity" label="未完成任务（件/吨）" align="center" width="150px">
              <template #default="{ row }">
                <div @click.stop="unComplete(row)">
                  <span class="tc-danger">{{ row.unQuantity }}</span>
                  <span> / </span>
                  <span class="tc-danger">{{ (row.unNetWeight / 1000)?.toFixed(2) }}</span>
                </div>
              </template>
            </el-table-column>
          </common-table>
          <el-divider class="title"><el-tag size="medium"> 零件 </el-tag></el-divider>
          <common-table
            ref="indirectRef"
            highlight-current-row
            :data="partGroupList"
            style="width: 100%; cursor: pointer"
            row-key="id"
            :max-height="maxHeight / 3 - 40"
            @row-click="handleRowClick"
          >
            <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
            <el-table-column prop="groups.name" key="groups.name" label="班组" align="center">
              <template #default="{ row }">
                <span>{{ row.groups?.name }}>{{ row.team?.name }}</span>
              </template>
            </el-table-column>
            <el-table-column prop="lastMonthNetWeight" key="lastMonthNetWeight" label="上月产量（吨）" align="center" width="120px">
              <template #default="{ row }">
                <span>{{ (row.lastMonthNetWeight / 1000)?.toFixed(2) }}</span>
              </template>
            </el-table-column>
            <el-table-column prop="yearNetWeight" key="yearNetWeight" label="年度平均产量（吨）" align="center" width="150px">
              <template #default="{ row }">
                <span>{{ (row.yearNetWeight / 1000)?.toFixed(2) }}</span>
              </template>
            </el-table-column>
            <el-table-column prop="unQuantity" key="unQuantity" label="未完成任务（件/吨）" align="center" width="150px">
              <template #default="{ row }">
                <div @click.stop="unComplete(row)">
                  <span class="tc-danger">{{ row.unQuantity }}</span>
                  <span> / </span>
                  <span class="tc-danger">{{ (row.unNetWeight / 1000)?.toFixed(2) }}</span>
                </div>
              </template>
            </el-table-column>
          </common-table>
        </div>
        <div style="border-right: 1px solid #ededed; margin: 0 20px; height: calc(100vh - 180px)"></div>
        <div style="width: 58%">
          <group-status-detail :detail-data="detailData" :workshopId="props.workshopId" :areaId="props.areaId" />
        </div>
      </div>
    </template>
  </common-drawer>
</template>

<script setup>
import { getGroupDialog } from '@/api/mes/production-monitoring-kanban/kanban.js'
import useVisible from '@compos/use-visible'
// import usePagination from '@compos/use-pagination'
import useMaxHeight from '@compos/use-max-height'
import { componentTypeEnum } from '@enum-ms/mes'
import { defineProps, defineEmits, ref, watch } from 'vue'
import groupStatusDetail from './group-status-detail.vue'

const emit = defineEmits(['update:visible'])
const detailData = ref([])
const groupList = ref([])
const artifactGroupList = ref([])
const assembleGroupList = ref([])
const partGroupList = ref([])

const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  groupDetailData: {
    type: Object,
    default: () => {}
  },
  workshopId: {
    type: Number
  },
  areaId: {
    type: Number
  }
})

// 高度
const { maxHeight } = useMaxHeight({
  mainBox: '.group-detail-drawer',
  extraBox: ['.el-drawer__header', '.title'],
  wrapperBox: ['.el-drawer__body'],
  navbar: false,
  clientHRepMainH: true,
  paginate: true
})

const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook: showHook })

function showHook() {
  detailData.value = {}
}

watch(
  [() => props.workshopId, () => props.areaId],
  (val) => {
    fetchGroupDetailGet()
  },
  { immediate: true }
)

async function fetchGroupDetailGet() {
  artifactGroupList.value = []
  assembleGroupList.value = []
  partGroupList.value = []
  try {
    const data = await getGroupDialog({
      workshopId: props.workshopId,
      areaId: props.areaId
    })
    groupList.value = data || []
    groupList.value?.forEach((v) => {
      if (v.taskTypeEnum === componentTypeEnum.ARTIFACT.V) {
        artifactGroupList.value.push(v)
      } else if (v.taskTypeEnum === componentTypeEnum.ASSEMBLE.V) {
        assembleGroupList.value.push(v)
      } else if (v.taskTypeEnum === componentTypeEnum.MACHINE_PART.V) {
        partGroupList.value.push(v)
      }
    })
  } catch (e) {
    console.log('获取班组状态失败', e)
  }
}

function unComplete(row) {
  detailData.value = row
}

function handleRowClick(val) {
  detailData.value = val
}
</script>

<style rel="stylesheet/scss" lang="scss" scoped>
</style>

