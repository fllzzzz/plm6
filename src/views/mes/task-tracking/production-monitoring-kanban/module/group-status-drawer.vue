<template>
  <common-drawer
    ref="drawerRef"
    customClass="group-detail-drawer"
    :title="`班组状态`"
    v-model="drawerVisible"
    direction="rtl"
    :before-close="handleClose"
    size="100%"
  >
    <template #titleAfter>
      <common-radio-button
        v-if="workshopId"
        v-model="processId"
        :options="processData"
        type="other"
        class="filter-item"
        default
        :dataStructure="{ key: 'id', label: 'name', value: 'id' }"
        size="small"
      />
    </template>
    <template #content>
      <div style="display: flex">
        <div style="width: 32%">
          <common-table
            ref="directRef"
            highlight-current-row
            :data="list"
            style="width: 100%; cursor: pointer"
            row-key="id"
            :max-height="maxHeight"
            @row-click="handleRowClick"
          >
            <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
            <el-table-column
              :show-overflow-tooltip="true"
              prop="groups.name"
              key="groups.name"
              label="产线/班组"
              align="center"
              min-width="120"
            >
              <template #default="{ row }">
                <span>{{ row.productionLine?.name }}>{{ row.groups?.name }}>{{ row.team?.name }}</span>
              </template>
            </el-table-column>
            <el-table-column
              :show-overflow-tooltip="true"
              prop="yearNetWeight"
              key="yearNetWeight"
              label="年度平均产量（吨）"
              align="center"
              width="150px"
            >
              <template #default="{ row }">
                <span>{{ (row.yearNetWeight / 1000)?.toFixed(2) }}</span>
              </template>
            </el-table-column>
            <el-table-column
              :show-overflow-tooltip="true"
              prop="lastMonthNetWeight"
              key="lastMonthNetWeight"
              label="上月产量（吨）"
              align="center"
              width="120px"
            >
              <template #default="{ row }">
                <span>{{ (row.lastMonthNetWeight / 1000)?.toFixed(2) }}</span>
              </template>
            </el-table-column>
            <el-table-column
              :show-overflow-tooltip="true"
              prop="unNetWeight"
              key="unNetWeight"
              label="在手任务（吨）"
              align="center"
              width="120px"
            >
              <template #default="{ row }">
                <span class="tc-danger">{{ (row.unNetWeight / 1000)?.toFixed(2) }}</span>
              </template>
            </el-table-column>
            <!-- <el-table-column prop="unQuantity" key="unQuantity" label="未完成任务（件/吨）" align="center" width="150px">
              <template #default="{ row }">
                <div @click.stop="unComplete(row)">
                  <span class="tc-danger">{{ row.unQuantity }}</span>
                  <span> / </span>
                  <span class="tc-danger">{{ (row.unNetWeight / 1000)?.toFixed(2) }}</span>
                </div>
              </template>
            </el-table-column> -->
          </common-table>
        </div>
        <div style="border-right: 1px solid #ededed; margin: 0 20px; height: calc(100vh - 180px)"></div>
        <div style="width: 67%">
          <group-status-detail :detail-data="detailData" :workshopId="props.workshopId" :processId="processId" />
        </div>
      </div>
    </template>
  </common-drawer>
</template>

<script setup>
import { getGroupDialog } from '@/api/mes/production-monitoring-kanban/kanban.js'
import { getProcess } from '@/api/mes/factory-report/group-report.js'
import useVisible from '@compos/use-visible'
// import usePagination from '@compos/use-pagination'
import useMaxHeight from '@compos/use-max-height'
import { defineProps, defineEmits, ref, watch } from 'vue'
import groupStatusDetail from './group-status-detail.vue'

const emit = defineEmits(['update:visible'])
const detailData = ref([])
const list = ref([])
const processData = ref([])
const processId = ref()
const processMap = ref({})

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
  processId.value = undefined
  fetchProcess()
  fetchGroupDetailGet()
}

watch(
  () => props.workshopId,
  (val) => {
    fetchGroupDetailGet()
  },
  { immediate: true }
)

watch(
  () => props.workshopId,
  (val) => {
    fetchProcess()
  }
)

watch(
  () => processId.value,
  (val) => {
    if (val) {
      fetchGroupDetailGet()
    }
  }
)

// 获取工序
async function fetchProcess() {
  try {
    const data = await getProcess({
      workshopId: props.workshopId
    })
    console.log(data, 'data')
    processData.value = data || []
    data?.forEach((v) => {
      processMap[v.id] = v
    })
    console.log(processMap, 'processMap')
  } catch (error) {
    console.log('获取班组的工序失败')
  }
}

async function fetchGroupDetailGet() {
  list.value = []
  try {
    const data = await getGroupDialog({
      workshopId: props.workshopId,
      processId: processId.value,
      taskTypeEnum: processMap[processId.value]?.productType
    })
    list.value = data || []
  } catch (e) {
    console.log('获取班组状态失败', e)
  }
}

// function unComplete(row) {
//   detailData.value = row
// }

function handleRowClick(val) {
  detailData.value = val
}
</script>

<style rel="stylesheet/scss" lang="scss" scoped>
</style>

