<template>
  <div class="app-container">
    <!-- 工具栏 -->
    <mHeader @change="headerChange" />
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight - 50"
      style="width: 100%"
    >
      <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
      <template v-if="crud.data && transformTab === 0">
        <el-table-column key="workshopInf" align="center" prop="workshopInf" :show-overflow-tooltip="true" label="车间" min-width="40">
          <template v-slot="scope">
            <span>{{ scope.row.cutMachine.workshopInf }}</span>
          </template>
        </el-table-column>
        <el-table-column key="machineName" align="center" prop="machineName" :show-overflow-tooltip="true" label="设备名称" min-width="40">
          <template v-slot="scope">
            <el-tag style="width: 100%" effect="plain">
              <span>{{ scope.row.cutMachine.machineName }}</span>
            </el-tag>
          </template>
        </el-table-column>
      </template>
      <template v-else>
        <el-table-column key="projectName" :show-overflow-tooltip="true" label="项目名称" align="center" min-width="90">
          <template v-slot="scope">
            <span>{{ scope.row.projectName }}</span>
          </template>
        </el-table-column>
      </template>
      <el-table-column key="plateNum" align="center" prop="plateNum" :show-overflow-tooltip="true" label="任务数（张）" min-width="40">
        <template v-slot="scope">
          <span>{{ scope.row.plateNum ? scope.row.plateNum : 0 }}</span>
        </template>
      </el-table-column>
      <el-table-column
        key="plateWeight"
        align="center"
        prop="plateWeight"
        :show-overflow-tooltip="true"
        label="任务量（kg）"
        min-width="40"
      >
        <template v-slot="scope">
          <span>{{ scope.row.plateWeight ? scope.row.plateWeight : 0 }}</span>
        </template>
      </el-table-column>
      <el-table-column key="finishNum" align="center" prop="finishNum" :show-overflow-tooltip="true" label="完成数（张）" min-width="40">
        <template v-slot="scope">
          <span>{{ scope.row.finishNum ? scope.row.finishNum : 0 }}</span>
        </template>
      </el-table-column>
      <el-table-column
        key="finishWeight"
        align="center"
        prop="finishWeight"
        :show-overflow-tooltip="true"
        label="完成量（kg）"
        min-width="40"
      >
        <template v-slot="scope">
          <span>{{ scope.row.finishWeight ? scope.row.finishWeight : 0 }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="transformTab === 0 && checkPermission(permission.detailMachine)" label="操作" align="center" min-width="90">
        <template v-slot="scope">
          <common-button type="primary" size="mini" @click="showDetail(scope.row)">查 看</common-button>
        </template>
      </el-table-column>
      <el-table-column
        v-else-if="transformTab === 1 && checkPermission(permission.detailProject)"
        label="操作"
        align="center"
        min-width="90"
      >
        <template v-slot="scope">
          <common-button icon="el-icon-view" type="primary" size="mini" @click="Detail(scope.row)" />
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />

    <detail @colesHook="colesHook" :detail-data="detailObj" v-model:visible="innerVisible" />
    <detail-load :detail-data="loadObj" v-model:visible="loadObjVisible" />
  </div>
</template>

<script setup>
import { ref } from 'vue'
import crudApi from '@/api/cutting/scheduling'
import nestingList from '@/api/cutting/nestingList'
import useCRUD from '@compos/use-crud'
import mHeader from './module/header'
import detail from './module/detail/index.vue'
import detailLoad from '@/views/cutting/template/steel-plate-list.vue'
import pagination from '@crud/Pagination'
import useMaxHeight from '@compos/use-max-height'
import checkPermission from '@/utils/system/check-permission'
import { cuttingSchedulingPM as permission } from '@/page-permission/cutting'

const tableRef = ref()
const transformTab = ref(0)
const innerVisible = ref(false)
const detailObj = ref({})

const loadObj = ref({})
const loadObjVisible = ref(false)

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const { crud } = useCRUD(
  {
    title: '切割排产',
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

async function showDetail(row) {
  detailObj.value = row
  innerVisible.value = true
}

function Detail(row) {
  loadObj.value = row
  loadObjVisible.value = true
}

function headerChange(val) {
  transformTab.value = val
  if (transformTab.value === 1) {
    crud.crudApi = { ...nestingList }
  } else {
    crud.crudApi = { ...crudApi }
  }
  crud.data = []
  crud.toQuery()
}

function colesHook() {
  crud.toQuery()
}

</script>
