<template>
  <div class="app-container wrap">
    <div class="wrap-left">
      <div class="head-container">
        <mHeader />
      </div>
      <!--表格渲染-->
      <common-table
        ref="tableRef"
        v-loading="crud.loading"
        :data="crud.data"
        :empty-text="crud.emptyText"
        :max-height="maxHeight"
        :data-format="dataFormat"
        style="width: 100%"
        highlight-current-row
        @current-change="handleClickChange"
      >
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column
          v-if="columns.visible('orderNumber')"
          align="center"
          prop="orderNumber"
          :show-overflow-tooltip="true"
          label="排产工单号"
          min-width="120px"
        />
        <el-table-column
          v-if="columns.visible('project')"
          prop="project"
          :show-overflow-tooltip="true"
          label="所属项目"
          min-width="160px"
        />
        <el-table-column
          v-if="columns.visible('askCompleteTime')"
          align="center"
          prop="askCompleteTime"
          :show-overflow-tooltip="true"
          label="计划完成日期"
          width="120px"
        />
        <el-table-column
          v-if="columns.visible('totalMete')"
          align="center"
          prop="totalMete"
          :show-overflow-tooltip="true"
          label="任务量（件/kg）"
          width="135px"
        >
          <template #default="{ row }">
            <span>{{ row.totalMete?.quantity || 0 }} / {{ row.totalMete?.netWeight || 0 }}</span>
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('totalCompleteMete')"
          align="center"
          prop="totalCompleteMete"
          :show-overflow-tooltip="true"
          label="实际完成（件/kg）"
          width="135px"
        >
          <template #default="{ row }">
            <span>{{ row.totalCompleteMete?.quantity || 0 }} / {{ row.totalCompleteMete.netWeight || 0 }}</span>
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('completeRatio')"
          align="center"
          prop="completeRatio"
          :show-overflow-tooltip="true"
          label="完成率"
          width="100px"
        >
          <template #default="{ row }">
            <span>{{ row.completeRatio }}%</span>
          </template>
        </el-table-column>
      </common-table>
      <!--分页组件-->
      <pagination />
    </div>
    <el-divider direction="vertical" :style="`height: ${maxHeight + 90}px`"></el-divider>
    <div class="wrap-right" :style="`height: ${maxHeight + 90}px;overflow: auto;`">
      <el-tag v-if="!currentInfo?.id" type="info" size="medium"> * 请先选择排产工单，进行班组协同 </el-tag>
      <template v-else>
        <div v-for="item in taskList" :key="item.id" style="margin-bottom: 10px">
          <div class="head-container">
            <el-tag effect="dark" :type="componentTypeTag[componentTypeEnum.VK[item?.taskTypeEnum]]">
              {{ componentTypeEnum.VL[item?.taskTypeEnum] }}
            </el-tag>
            <el-tag style="margin-left: 8px" effect="plain"> {{ item?.productionLine?.name }}>{{ item?.groups?.name }} </el-tag>
            <span style="margin-left: 8px; font-size: 14px">工单号：{{ item?.orderNumber }}</span>
          </div>
          <common-table ref="tableRef" :data="item.processList" :empty-text="'暂无数据'" style="width: 100%; cursor: pointer">
            <el-table-column align="center" prop="process.name" :show-overflow-tooltip="true" label="工序" />
            <el-table-column
              align="center"
              key="completeRatio"
              prop="completeRatio"
              :show-overflow-tooltip="true"
              label="进度"
              width="160px"
            >
              <template #default="{ row }">
                <el-progress
                  :text-inside="true"
                  stroke-linecap="square"
                  :stroke-width="22"
                  :percentage="Number(row.completeRatio)"
                  status="success"
                />
              </template>
            </el-table-column>
            <el-table-column align="center" prop="totalTaskMete.quantity" :show-overflow-tooltip="true" label="任务（件/kg）">
              <template #default="{ row }">
                <span>{{ row.totalTaskMete?.quantity || 0 }}/{{ row.totalTaskMete?.netWeight || 0 }}</span>
              </template>
            </el-table-column>
            <el-table-column align="center" prop="totalCompleteMete.quantity" :show-overflow-tooltip="true" label="完成（件/kg）">
              <template #default="{ row }">
                <span>{{ row.totalCompleteMete?.quantity || 0 }}/{{ row.totalCompleteMete?.netWeight || 0 }}</span>
              </template>
            </el-table-column>
            <el-table-column label="操作" width="120px" align="center" fixed="right">
              <template #default="{ row }">
                <common-button size="mini" type="primary" @click="toAssistance(row, item)">班组协同</common-button>
              </template>
            </el-table-column>
          </common-table>
        </div>
      </template>
    </div>
    <assistance-drawer v-model:visible="assistanceVisible" :info="itemInfo" @success="crud.toQuery" />
  </div>
</template>

<script setup>
import crudApi, { getTask } from '@/api/mes/task-tracking/assistance-operate/process-assistance'
import { ref } from 'vue'

import { componentTypeEnum } from '@enum-ms/mes'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import assistanceDrawer from './module/assistance-drawer.vue'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const componentTypeTag = {
  [componentTypeEnum.ARTIFACT.K]: 'success',
  [componentTypeEnum.ASSEMBLE.K]: 'warning',
  [componentTypeEnum.MACHINE_PART.K]: ''
}

const dataFormat = ref([
  ['askCompleteTime', ['parse-time', '{y}-{m}-{d}']],
  ['project', 'parse-project']
])

const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '工序协同',
    sort: [],
    permission: { get: [''] },
    optShow: { ...optShow },
    crudApi: { ...crudApi }
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true })

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content.map((v) => {
    v.completeRatio =
      (v.totalMete?.netWeight &&
        v.totalCompleteMete?.netWeight &&
        ((v.totalCompleteMete?.netWeight / v.totalMete?.netWeight) * 100).toFixed(2)) ||
      0
    return v
  })
}

const currentInfo = ref({})
const taskList = ref([])
const taskLoading = ref(false)

function handleClickChange(val) {
  currentInfo.value = Object.assign({}, val)
  fetchTask(val?.id)
}

async function fetchTask(id) {
  taskList.value = []
  if (!id) return
  try {
    taskLoading.value = true
    const { content } = await getTask({ topTaskOrderId: id })
    taskList.value = content.map((v) => {
      v.processList.forEach((o) => {
        o.completeRatio =
          (o.totalTaskMete?.netWeight &&
            o.totalCompleteMete?.netWeight &&
            ((o.totalCompleteMete?.netWeight / o.totalTaskMete?.netWeight) * 100).toFixed(2)) ||
          0
      })
      return v
    })
  } catch (er) {
    console.log('获取子工单失败', er)
  } finally {
    taskLoading.value = false
  }
}

const assistanceVisible = ref(false)
const itemInfo = ref({})

function toAssistance(row, item) {
  itemInfo.value = Object.assign({}, row, item, { topTaskOrderId: currentInfo.value?.id })
  assistanceVisible.value = true
}
</script>

<style lang="scss" scoped>
.wrap {
  display: flex;
  .wrap-left {
    width: 60%;
  }
  .wrap-right {
    flex: 1;
  }
}
::-webkit-scrollbar {
  width: 6px;
  height: 6px;
}
::-webkit-scrollbar-thumb {
  border-radius: 6px;
}
</style>
