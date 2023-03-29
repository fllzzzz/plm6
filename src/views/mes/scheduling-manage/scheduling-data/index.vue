<template>
  <div class="app-container">
    <div class="head-container">
      <mHeader />
    </div>
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :data-format="dataFormat"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      style="width: 100%"
      class="collection-table"
      :stripe="false"
      :showEmptySymbol="false"
    >
      <el-table-column prop="index" label="序号" align="center" width="50" type="index" />
      <el-table-column
        v-if="columns.visible('project')"
        key="project"
        prop="project"
        label="所属项目"
        width="130"
        :show-overflow-tooltip="true"
      />
      <el-table-column
        v-if="columns.visible('monomer.name')"
        key="monomer.name"
        prop="monomer.name"
        label="单体"
        align="center"
        :show-overflow-tooltip="true"
      />
      <el-table-column
        v-if="columns.visible('scheduleMete') && crud.query.timeType === timeTypeEnum.CURRENT_MONTH.V"
        key="scheduleMete"
        prop="scheduleMete"
        label="排产量"
        align="center"
        :show-overflow-tooltip="true"
      />
      <el-table-column
        v-if="columns.visible('actualMete') && crud.query.timeType === timeTypeEnum.CURRENT_MONTH.V"
        key="actualMete"
        prop="actualMete"
        label="实际完成"
        align="center"
        :show-overflow-tooltip="true"
      />
      <el-table-column
        v-if="columns.visible('completeRate') && crud.query.timeType === timeTypeEnum.CURRENT_MONTH.V"
        key="completeRate"
        prop="completeRate"
        label="完成率"
        align="center"
        :show-overflow-tooltip="true"
      />
      <!-- <template><template> -->
      <el-table-column align="center" label="排产计划及执行（单位：吨）" :show-overflow-tooltip="true" v-if="crud.query.timeType === timeTypeEnum.ALL_YEAR.V">
        <template v-for="item in monthArr" :key="item">
          <el-table-column :label="item" align="center" :show-overflow-tooltip="true">
            <template #default="{ row }">
              <span>{{ row.quantity }}</span>
            </template>
          </el-table-column>
        </template>
      </el-table-column>
    </common-table>
  </div>
</template>

<script setup>
// import crudApi from '@/api/mes/production-order-manage/production-order'
import { ref, provide } from 'vue'
import { timeTypeEnum } from '@enum-ms/contract'
// import { parseTime } from '@/utils/date'
import { mesScheduleDetailPM as permission } from '@/page-permission/mes'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import mHeader from './module/header'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const monthArr = ref([])
for (let i = 1; i <= 12; i++) {
  monthArr.value.push(i)
}

const dataFormat = ref([['project', 'parse-project']])

const tableRef = ref()

const { crud, columns, CRUD } = useCRUD(
  {
    title: '排产数据',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    invisibleColumns: [],
    // crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)

provide('permission', permission)
const { maxHeight } = useMaxHeight({
  paginate: true,
  extraHeight: 40
})

CRUD.HOOK.handleRefresh = (crud, { data }) => {
  data.content = data.content?.map((v) => {
    return v
  })
}
</script>

<style lang="scss" scoped>
.collection-table {
  ::v-deep(.el-select .el-input__inner) {
    padding-left: 2px;
    padding-right: 5px;
  }
  ::v-deep(.el-input-number .el-input__inner, .el-input__inner) {
    text-align: left;
    padding: 0 5px;
  }
  ::v-deep(.el-table .cell) {
    padding-left: 2px;
    padding-right: 2px;
  }
}
</style>
