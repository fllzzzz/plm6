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
    <div class="wrap-right">
      <el-tag type="info" size="medium"> * 请先选择项目，进行班组协同 </el-tag>
    </div>
  </div>
</template>

<script setup>
import crudApi from '@/api/mes/task-tracking/assistance-operate/process-assistance'
import { ref } from 'vue'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'

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
    title: '',
    sort: [],
    // permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi }
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true })

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content.map((v) => {
    v.completeRatio = (v.totalMete?.netWeight && ((v.totalCompleteMete?.netWeight / v.totalMete?.netWeight) * 100).toFixed(2)) || 0
    return v
  })
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
    overflow: hidden;
    min-width: 0;
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
