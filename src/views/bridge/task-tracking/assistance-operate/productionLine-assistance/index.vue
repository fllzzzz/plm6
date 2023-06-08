<template>
  <div class="app-container">
    <div class="head-container">
      <mHeader>
        <template #viewLeft>
          <common-button v-permission="permission.record" size="mini" type="info" icon="el-icon-time" @click="recordVisible = true">协同记录</common-button>
        </template>
      </mHeader>
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
      <template v-if="!(crud.query.taskTypeEnum & bridgeTaskTypeEnum.MACHINE_PART.V)">
        <el-table-column
          v-if="columns.visible('project')"
          prop="project"
          :show-overflow-tooltip="true"
          label="所属项目"
          min-width="160px"
        />
        <el-table-column
          v-if="columns.visible('monomer.name')"
          align="center"
          prop="monomer.name"
          :show-overflow-tooltip="true"
          label="单体"
          min-width="120px"
        />
        <el-table-column
          v-if="columns.visible('group.name')"
          prop="group.name"
          :show-overflow-tooltip="true"
          label="原生产组"
          min-width="160px"
        >
          <template #default="{ row }">
            <span>{{ row.workshop?.name }}>{{ row.productionLine?.name }}>{{ row.groups?.name }}</span>
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('askCompleteTime')"
          align="center"
          prop="askCompleteTime"
          :show-overflow-tooltip="true"
          label="计划完成日期"
          width="120px"
        />
      </template>
      <template v-if="crud.query.taskTypeEnum & bridgeTaskTypeEnum.MACHINE_PART.V">
        <el-table-column
          v-if="columns.visible('cutConfig.name')"
          prop="cutConfig.name"
          :show-overflow-tooltip="true"
          label="切割方式"
          width="135px"
          align="center"
        />
      </template>
      <el-table-column
        v-if="columns.visible('quantity')"
        align="center"
        prop="quantity"
        :show-overflow-tooltip="true"
        label="未完成量（件/kg）"
        width="135px"
      >
        <template #default="{ row }">
          <span>{{ row.quantity || 0 }} / {{ row.totalNetWeight || 0 }}</span>
        </template>
      </el-table-column>
      <!-- <el-table-column
        v-if="columns.visible('completeQuantity')"
        align="center"
        prop="completeQuantity"
        :show-overflow-tooltip="true"
        label="实际完成（件/kg）"
        width="135px"
      >
        <template #default="{ row }">
          <span>{{ row.completeQuantity }} / {{ row.totalCompleteNetWeight }}</span>
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
      </el-table-column> -->
      <el-table-column
        v-if="columns.visible('user.name') && !(crud.query.taskTypeEnum & bridgeTaskTypeEnum.MACHINE_PART.V)"
        align="center"
        prop="user.name"
        :show-overflow-tooltip="true"
        label="排产人"
        width="120px"
      />
      <el-table-column label="操作" width="120px" align="center" fixed="right">
        <template #default="{ row }">
          <common-button size="mini" type="primary" @click="toAssistance(row)">产线协同</common-button>
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <assistance-drawer v-model:visible="assistanceVisible" :info="itemInfo" @success="crud.toQuery" />
    <record-drawer v-model:visible="recordVisible"></record-drawer>
  </div>
</template>

<script setup>
import crudApi from '@/api/bridge/bridge-task-tracking/assistance-operate/productionLine-assistance'
import { ref } from 'vue'

import { bridgeTaskTypeEnum } from '@enum-ms/bridge'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import assistanceDrawer from './module/assistance-drawer.vue'
import recordDrawer from './module/record-drawer.vue'
import { bridgeProductionLineAssistancePM as permission } from '@/page-permission/bridge'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const dataFormat = ref([
  ['askCompleteTime', ['parse-time', '{y}-{m}-{d}']],
  ['project', 'parse-project']
])

const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '产线协同',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi }
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true })

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content.map((v) => {
    v.completeRatio = (v.totalNetWeight && ((v.totalCompleteNetWeight / v.totalNetWeight) * 100).toFixed(2)) || 0
    return v
  })
}

const recordVisible = ref(false)
const assistanceVisible = ref(false)
const itemInfo = ref({})

function toAssistance(row) {
  itemInfo.value = Object.assign({}, row)
  assistanceVisible.value = true
}
</script>
