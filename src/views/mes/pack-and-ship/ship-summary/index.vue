<template>
  <div class="app-container">
    <!--表格渲染-->
    <div style="display:flex;">
      <div style="width:40%;padding-right:10px;">
        <mHeader />
        <common-table
          ref="tableRef"
          v-loading="crud.loading"
          :data="crud.data"
          :data-format="dataFormat"
          :empty-text="crud.emptyText"
          @current-change="handleCurrentChange"
          highlight-current-row
          :max-height="maxHeight"
          style="width: 100%;margin-top:10px;"
          class="collection-table"
          :stripe="false"
          :showEmptySymbol="false"
        >
          <el-table-column prop="index" label="序号" align="center" width="45" type="index" />
          <el-table-column key="project" prop="project" label="项目" align="left" show-overflow-tooltip min-width="120"/>
          <el-table-column key="artifactRate" prop="artifactRate" label="构件发运数据(t)" align="center" min-width="160" show-overflow-tooltip>
            <template v-slot="scope">
              <div style="position:relative;">
                <el-progress :stroke-width="16" :percentage="scope.row.artifactRate"/>
                <span style="position:absolute;top:-2px;left:0;width:100%;text-align:right;padding-right:60px;">{{ crud.query.year? (scope.row.artifactShipMete.toFixed(2) +' | ' + scope.row.artifactMete.toFixed(2)) : (scope.row.totalArtifactShipMete.toFixed(2) +' | ' +scope.row.artifactMete.toFixed(2)) }}</span>
              </div>
            </template>
          </el-table-column>
          <el-table-column key="enclosureRate" prop="enclosureRate" label="围护发运数据(m)" align="center" min-width="160" show-overflow-tooltip>
            <template v-slot="scope">
              <div style="position:relative;">
                <el-progress :stroke-width="16" :percentage="scope.row.enclosureRate" :color="'#67c23a'"/>
                <span style="position:absolute;top:-2px;left:0;width:100%;text-align:right;padding-right:60px;">{{ crud.query.year? (scope.row.enclosureShipMete.toFixed(2) +' | ' + scope.row.enclosureMete.toFixed(2)) : (scope.row.totalEnclosureShipMete.toFixed(2) +' | ' +scope.row.enclosureMete.toFixed(2)) }}</span>
              </div>
            </template>
          </el-table-column>
        </common-table>
      </div>
      <div style="border-right: 1px solid #ededed; height: calc(100vh - 120px)"></div>
      <div style="width:59%;padding-left:10px;">
        <projectShipmentDetail :currentRow="currentRow" v-if="isNotBlank(currentRow)" />
        <div class="my-code" v-else>*点击左表操作查看明细</div>
      </div>
    </div>
  </div>
</template>

<script setup>
import crudApi from '@/api/mes/pack-and-ship/ship-summary'
import { ref } from 'vue'

// import { productShipmentPM as permission } from '@/page-permission/project'
import { isNotBlank } from '@data-type/index'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import mHeader from './module/header'
import projectShipmentDetail from './module/project-shipment-detail.vue'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const dataFormat = ref([
  ['project', 'parse-project']
])

const tableRef = ref()
const currentRow = ref({})

const { crud, CRUD } = useCRUD(
  {
    title: '发运管理',
    sort: [],
    // permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    hasPagination: false
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  paginate: true,
  extraHeight: 40
})

CRUD.HOOK.handleRefresh = (crud, { data }) => {
  data.content.forEach(v => {
    v.projectId = v.project?.id
    v.artifactRate = crud.query.year ? (v.artifactShipMete && v.artifactMete ? ((v.artifactShipMete / v.artifactMete) * 100).toFixed(2) : 0) : (v.totalArtifactShipMete && v.artifactMete ? ((v.totalArtifactShipMete / v.artifactMete) * 100).toFixed(2) : 0)
    v.enclosureRate = crud.query.year ? (v.enclosureShipMete && v.enclosureMete ? ((v.enclosureShipMete / v.enclosureMete) * 100).toFixed(2) : 0) : (v.totalEnclosureShipMete && v.enclosureMete ? ((v.totalEnclosureShipMete / v.enclosureMete) * 100).toFixed(2) : 0)
  })
}

function handleCurrentChange(val) {
  currentRow.value = val
}

</script>

<style lang="scss" scoped>
.collection-table{
  ::v-deep(.el-select .el-input__inner){
    padding-left:2px;
    padding-right:5px;
  }
  ::v-deep(.el-input-number .el-input__inner, .el-input__inner) {
    text-align: left;
    padding:0 5px;
  }
  ::v-deep(.el-table .cell){
    padding-left:2px;
    padding-right:2px;
  }
}
::v-deep(.el-table .cell){
  padding-left:3px;
  padding-right:0;
}
::v-deep(.el-progress__text){
  font-size:12px !important;
}
</style>
