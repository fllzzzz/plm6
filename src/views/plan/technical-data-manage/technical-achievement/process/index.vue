<template>
  <div class="app-container">
    <!-- 统一查询条件 -->
    <div style="margin-bottom:15px;padding-bottom:10px;border-bottom:1px solid #eee;">
      <monomer-select
        ref="monomerSelectRef"
        v-model="query.monomerId"
        :project-id="globalProjectId"
        class="filter-item"
        @getAreaInfo="getAreaInfo"
      />
      <common-select
        v-model="query.areaId"
        :options="areaInfo"
        type="other"
        :dataStructure="{ key: 'id', label: 'name', value: 'id' }"
        size="small"
        clearable
        placeholder="请选择区域"
        class="filter-item"
        style="width:200px;margin-left:10px;"
      />
    </div>
    <div class="wrap">
      <div class="wrap-left">
        <structure-type />
      </div>
      <div class="wrap-center">
          <!--工具栏-->
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
            return-source-data
            :showEmptySymbol="false"
            style="width: 100%"
            @selection-change="crud.selectionChangeHandler"
          >
          <el-table-column type="selection" width="55" align="center" />
          <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
          <el-table-column key="createTime" prop="createTime" label="日期" align="center">
            <template v-slot="scope">
              <div>{{ scope.row.createTime? parseTime(scope.row.createTime,'{y}-{m}-{d}'): '-' }}</div>
            </template>
          </el-table-column>
          <el-table-column v-if="columns.visible('createUserName')" key="createUserName" prop="createUserName" :show-overflow-tooltip="true" label="导入人" align="center"/>
          <el-table-column v-if="columns.visible('fileName')" key="fileName" prop="fileName" :show-overflow-tooltip="true" label="文件" align="center"/>
          <!--编辑与删除-->
          <el-table-column
            v-if="checkPermission([...permission.edit, ...permission.download])"
            label="操作"
            width="150px"
            align="center"
            fixed="right"
          >
            <template v-slot="scope">
              <!-- <common-button size="mini" type="primary" @click="editRow(scope.row)" v-permission="permission.edit">替换</common-button> -->
              <!-- 下载 -->
              <!-- <e-operation :data="scope.row" :permission="permission.download" /> -->
            </template>
          </el-table-column>
        </common-table>
        <!--分页组件-->
        <pagination />
      </div>
      <div class="wrap-right">
        <technology-file />
      </div>
    </div>
  <uploadForm v-model="uploadVisible" :currentMonomer="currentMonomer" :globalProject="globalProject" :dataType="crud.query.dataType" @success="crud.toQuery" :currentRow="currentRow"/>
  </div>
</template>

<script setup>
import crudApi from '@/api/plan/technical-data-manage/deepen'
import { ref, watch } from 'vue'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import { mapGetters } from '@/store/lib'
import checkPermission from '@/utils/system/check-permission'
import { parseTime } from '@/utils/date'
import { changeFileListPM as permission } from '@/page-permission/plan'

import uploadForm from './module/upload-form'
// import eOperation from '@crud/E.operation'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import structureType from './module/structure-type'
import technologyFile from './module/technology-file'
import monomerSelect from '@/components-system/plan/monomer-select'

const { globalProjectId, globalProject } = mapGetters(['globalProjectId', 'globalProject'])

const optShow = {
  add: false,
  edit: false,
  del: true,
  download: false
}

const tableRef = ref()
const currentRow = ref({})
const currentMonomer = ref({})
const uploadVisible = ref(false)
const query = ref({})
const areaInfo = ref([])
const { crud, columns, CRUD } = useCRUD(
  {
    title: '工艺文件',
    sort: ['id.desc'],
    permission: { ...permission },
    optShow: { ...optShow },
    requiredQuery: ['dataType'],
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.plan-process',
  paginate: true,
  extraHeight: 40
})

watch(
  () => globalProjectId.value,
  (val) => {
    if (val) {
      crud.toQuery()
    }
  },
  { immediate: true, deep: true }
)

function getAreaInfo(val) {
  areaInfo.value = val || []
}

function currentChange(val) {
  currentMonomer.value = val
}

function handleUpload() {
  currentRow.value = {}
  uploadVisible.value = true
}

CRUD.HOOK.beforeRefresh = () => {
  crud.query.projectId = globalProjectId.value
  return crud.query.projectId
}

CRUD.HOOK.handleRefresh = (crud, data) => {
  data.data.content = data.data.content.map(v => {
    v.edit = false
    v.originalRemark = v.remark
    v.editLoading = false
    return v
  })
}
</script>

<style lang="scss" scoped>
::v-deep(.abnormal-row) {
  background: #e8f4ff;
}
::v-deep(.hidden-select) {
  td:nth-child(1){
    .cell{
      opacity:0;
    }
  }
}
$font-size: 1.5em;
.child {
  width: $font-size;
  height: $font-size;
  display: inline-block;
  border: 1px solid;
  border-radius: 50%;
  line-height: $font-size;
}
.wrap{
  display:flex;
  .wrap-left{
    width:260px;
  }
  .wrap-center{
    flex:1;
    padding:0 16px;
  }
  .wrap-right{
     width:280px;
  }
}
</style>
