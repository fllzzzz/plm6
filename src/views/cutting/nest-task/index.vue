<template>
  <div class="app-container">
    <!-- 工具栏 -->
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
      style="width: 100%"
    >
      <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
      <el-table-column
        v-if="columns.visible('projectName')"
        key="projectName"
        prop="projectName"
        :show-overflow-tooltip="true"
        label="项目名称"
        min-width="60"
      >
        <template v-slot="scope">
          <span>{{ scope.row.projectName }}</span>
        </template>
      </el-table-column>
      <!-- <el-table-column
        v-if="columns.visible('monomerName')"
        key="monomerName"
        prop="monomerName"
        :show-overflow-tooltip="true"
        label="单体"
        min-width="60"
      >
        <template v-slot="scope">
          <span>{{ scope.row.monomerName }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('areaName')"
        key="areaName"
        prop="areaName"
        :show-overflow-tooltip="true"
        label="单元"
        min-width="60"
      >
        <template v-slot="scope">
          <span>{{ scope.row.areaName }}</span>
        </template>
      </el-table-column> -->
      <el-table-column
        v-if="columns.visible('sum')"
        key="sum"
        prop="sum"
        :show-overflow-tooltip="true"
        label="零件数量（件）"
        min-width="60"
      >
        <template v-slot="scope">
          <span>{{ scope.row.sum }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('reduce')"
        key="reduce"
        prop="reduce"
        :show-overflow-tooltip="true"
        label="零件重量（kg）"
        min-width="60"
      >
        <template v-slot="scope">
          <span>{{ scope.row.reduce }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('nestingState')"
        key="nestingState"
        prop="nestingState"
        :show-overflow-tooltip="true"
        label="状态"
        align="center"
        min-width="60"
      >
        <template v-slot="scope">
          <el-tag style="width: 100%" v-if="scope.row.nestingState === 0" type="primary">未排套</el-tag>
          <el-tag style="width: 100%" v-if="scope.row.nestingState && scope.row.nestingState === 1" type="warning">部分排套</el-tag>
          <el-tag style="width: 100%" v-if="scope.row.nestingState && scope.row.nestingState === 2" type="danger">排套结束</el-tag>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('importTime')"
        key="importTime"
        prop="importTime"
        :show-overflow-tooltip="true"
        label="导入时间"
        min-width="60"
      >
        <template v-slot="scope">
          <span> {{ parseTime(scope.row.importTime) }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('updateTime')" key="updateTime" prop="updateTime" label="编辑日期" width="140px">
        <template v-slot="scope">
          <span>{{ parseTime(scope.row.updateTime) }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('createTime')" key="createTime" prop="createTime" label="创建日期" width="140px">
        <template v-slot="scope">
          <span>{{ parseTime(scope.row.createTime) }}</span>
        </template>
      </el-table-column>
      <el-table-column :show-overflow-tooltip="true" label="操作" align="center">
        <template v-slot="scope">
          <common-button size="mini" type="primary" icon="el-icon-view" @click="viewDetails(scope.row)">查看</common-button>
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <detail :detail-data="detailObj" v-model:visible="specsVisible" />
  </div>
</template>

<script setup>
import { ref } from 'vue'
import crudApi from '@/api/cutting/radan-controller'
import useCRUD from '@compos/use-crud'
import mHeader from './module/header'
import { parseTime } from '@/utils/date'
import pagination from '@crud/Pagination'
import detail from './detail/index.vue'
import useMaxHeight from '@compos/use-max-height'

const specsVisible = ref(false)
const tableRef = ref()
const detailObj = ref([])

// crud交由presenter持有
const permission = {
  get: ['contractRecord:get']
}

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const { crud, columns } = useCRUD(
  {
    title: '项目数据',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.contractRecord',
  paginate: true,
  extraHeight: 40
})

// 查看详情
function viewDetails(row) {
  specsVisible.value = true
  detailObj.value = row
  console.log(row)
}

</script>
