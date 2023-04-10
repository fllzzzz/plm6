<template>
  <div class="app-container wrap">
    <div class="head-container">
      <!-- <mHeader /> -->
      <el-date-picker
        v-model="date"
        type="daterange"
        range-separator=":"
        size="small"
        value-format="x"
        :clearable="false"
        :shortcuts="PICKER_OPTIONS_SHORTCUTS"
        unlink-panels
        start-placeholder="开始日期"
        end-placeholder="结束日期"
        style="width: 240px; margin-right: 10px"
        class="filter-item date-item"
        @change="handleDateChange"
      />
      <workshop-select
        v-model="workshopId"
        placeholder="请选择车间"
        :factory-id="factoryId"
        style="width: 200px"
        class="filter-item"
        :clearable="true"
        @change="handleWorkshopChange"
      />
      <common-radio-button
        v-if="workshopId"
        v-model="processType"
        :options="processData"
        type="other"
        class="filter-item"
        :dataStructure="{ key: 'id', label: 'name', value: 'id' }"
        showOptionAll
        size="small"
        @change="fetchProcessList"
      />
    </div>
    <div class="wrap-content">
      <div class="wrap-left">
        <process-list ref="processListRef" :maxHeight="maxHeight - 40" @nesting-task-click="handleNestingTaskClick" />
      </div>
      <div class="wrap-right">
        <el-tag v-if="!crud.query?.groupsId" type="info" size="medium"> * 请点击左侧工序列表查看详情 </el-tag>
        <template v-else>
          <div style="display: flex; justify-content: space-between; margin-bottom: 8px">
            <div>
              <el-tag size="medium">车间：{{ info?.workshop?.name }}>{{ info?.groups?.name }}</el-tag>
              <el-tag size="medium" style="margin-left: 10px">工序：{{ info?.process?.name }}</el-tag>
            </div>
            <crudOperation>
              <template #optRight>
                <print-table
                  v-permission="permission.print"
                  api-key="mesGroupsReport"
                  :params="{
                    processId: crud.query.processId,
                    workshopId: workshopId,
                    startDate: startDate,
                    endDate: endDate,
                    taskTypeEnum: crud.query.taskTypeEnum,
                    groupsId: crud.query.groupsId,
                    teamId: crud.query.teamId,
                  }"
                  size="mini"
                  type="warning"
                  class="filter-item"
                  style="width: 300px"
                />
              </template>
            </crudOperation>
          </div>
          <!--表格渲染-->
          <common-table
            ref="tableRef"
            v-loading="crud.loading"
            :data="crud.data"
            :empty-text="crud.emptyText"
            :dataFormat="dataFormat"
            :max-height="maxHeight - 50"
            :show-empty-symbol="false"
            style="width: 100%"
          >
            <el-table-column label="序号" type="index" align="center" width="70" />
            <el-table-column v-if="columns.visible('project')" :show-overflow-tooltip="true" label="项目" type="project" align="center" min-width="120">
              <template #default="{ row }">
                <span>{{ row.project?.serialNumber }}-{{ row.project?.name }}</span>
              </template>
            </el-table-column>
            <el-table-column v-if="columns.visible('monomer.name')" :show-overflow-tooltip="true" prop="monomer.name" label="单体" align="center">
              <template #default="{ row }">
                <span>{{ row.monomer ? row.monomer?.name : '/' }}</span>
              </template>
            </el-table-column>
            <el-table-column v-if="columns.visible('area.name')" :show-overflow-tooltip="true" prop="area.name" label="区域" align="center">
              <template #default="{ row }">
                <span>{{ row.area ? row.area?.name : '/' }}</span>
              </template>
            </el-table-column>
            <el-table-column v-if="columns.visible('serialNumber')" :show-overflow-tooltip="true" prop="serialNumber" label="编号" min-width="80px" align="center" />
            <el-table-column v-if="columns.visible('specification')" :show-overflow-tooltip="true" prop="specification" label="规格" min-width="80px" align="center" />
            <el-table-column v-if="columns.visible('length')" :show-overflow-tooltip="true" prop="length" label="长度" align="center" />
            <el-table-column v-if="columns.visible('quantity')" :show-overflow-tooltip="true" prop="quantity" label="数量" align="center" />
            <el-table-column v-if="columns.visible('netWeight')" :show-overflow-tooltip="true" prop="netWeight" label="单净重（kg）" align="center" />
            <el-table-column v-if="columns.visible('grossWeight')" :show-overflow-tooltip="true" prop="grossWeight" label="单毛重（kg）" align="center" />
          </common-table>
          <!--分页组件-->
          <pagination />
        </template>
      </div>
    </div>
  </div>
</template>

<script setup>
import crudApi from '@/api/mes/factory-report/group-report.js'
import { getProcessList, getProcess } from '@/api/mes/factory-report/group-report.js'
import { ref, watch, provide, onMounted } from 'vue'
import moment from 'moment'
import { PICKER_OPTIONS_SHORTCUTS } from '@/settings/config'
import workshopSelect from '@comp-mes/workshop-select'
import { mesGroupReportPM as permission } from '@/page-permission/mes'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import pagination from '@crud/Pagination'
import processList from './module/process-list.vue'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const info = ref({})
const { crud, columns, CRUD } = useCRUD(
  {
    title: '班组报表',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    invisibleColumns: ['grossWeight'],
    requiredQuery: ['groupsId']
  },
  tableRef
)

const dataFormat = ref([['scheduleTime', ['parse-time', '{y}-{m}-{d}']]])
const { maxHeight } = useMaxHeight({ paginate: true })

const processListRef = ref()
const date = ref([moment().startOf('month').valueOf(), moment().valueOf()])
const startDate = ref(moment().startOf('month').valueOf())
const endDate = ref(moment().valueOf())
const workshopId = ref()
const factoryId = ref()
const processType = ref()
const processData = ref([])
const tableData = ref([])
const loading = ref(false)

provide('tableData', tableData)
provide('loading', loading)

watch([() => workshopId.value, () => processType.value], (val) => {
  crud.query.groupsId = undefined
  fetchProcess()
  fetchProcessList()
})

onMounted(() => {
  fetchProcessList()
})
// 获取工序
async function fetchProcess() {
  try {
    const data = await getProcess({
      workshopId: workshopId.value
    })
    processData.value = data || []
  } catch (error) {
    console.log('获取班组的工序失败')
  }
}

async function fetchProcessList() {
  try {
    loading.value = true
    tableData.value = []
    const data = await getProcessList({
      startDate: startDate.value,
      endDate: endDate.value,
      processId: processType.value,
      workshopId: workshopId.value
    })
    tableData.value = data || []
  } catch (error) {
    console.log('获取工序列表错误', error)
  } finally {
    loading.value = false
  }
}

function handleWorkshopChange() {
  processType.value = undefined
  fetchProcess()
  fetchProcessList()
}

// 时间变动
async function handleDateChange(val) {
  if (val && val.length > 1) {
    startDate.value = val[0]
    endDate.value = val[1]
  } else {
    startDate.value = moment().startOf('month').valueOf()
    endDate.value = moment().valueOf()
  }
  fetchProcess()
  fetchProcessList()
}

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content.map((v) => {
    return v
  })
}

function handleNestingTaskClick(val) {
  crud.query.processId = val?.process?.id
  crud.query.workshopId = val?.workshop?.id
  crud.query.groupsId = val?.groups?.id
  crud.query.teamId = val?.team?.id
  crud.query.taskTypeEnum = val?.taskTypeEnum
  crud.query.startDate = startDate.value
  crud.query.endDate = endDate.value
  info.value = val
  crud.toQuery()
}
</script>
<style lang="scss" scoped>
.wrap {
  display: flex;
  flex-direction: column;
  .wrap-content {
    display: flex;
    .wrap-left {
      width: 500px;
      margin-right: 20px;
      overflow-x: auto;
    }
    .wrap-right {
      flex: 31;
      min-width: 0;
      overflow: hidden;
    }
  }
}
</style>
