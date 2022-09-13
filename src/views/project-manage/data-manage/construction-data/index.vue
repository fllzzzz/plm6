<template>
  <div class="app-container">
    <div v-if="globalProject?.businessType===businessTypeEnum.INSTALLATION.V">
      <!--工具栏-->
      <div class="head-container">
        <mHeader :project-id="globalProjectId" @currentChange="currentChange" @handleUpload="handleUpload"/>
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
        <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
        <el-table-column v-if="columns.visible('constructionStageType')" key="constructionStageType" prop="constructionStageType" :show-overflow-tooltip="true" label="阶段" align="center"/>
        <el-table-column v-if="columns.visible('fileName')" key="fileName" prop="fileName" :show-overflow-tooltip="true" label="文件名称" align="center"/>
        <el-table-column v-if="columns.visible('userName')" key="userName" prop="userName" :show-overflow-tooltip="true" label="上传人" align="center"/>
        <el-table-column key="createTime" prop="createTime" label="日期" align="center">
          <template v-slot="scope">
            <div>{{ scope.row.createTime? parseTime(scope.row.createTime,'{y}-{m}-{d}'): '-' }}</div>
          </template>
        </el-table-column>
        <!--编辑与删除-->
        <el-table-column
          label="操作"
          width="150px"
          align="center"
          fixed="right"
        >
          <template v-slot="scope">
            <udOperation :data="scope.row" :permission="permission" :show-edit="false"/>
            <!-- 下载 -->
            <export-button :params="{id: scope.row.attachmentId}"/>
          </template>
        </el-table-column>
      </common-table>
      <!--分页组件-->
      <pagination />
    </div>
    <div v-else>
      <el-tag type="danger" size="medium" style="margin-bottom: 10px"> * 您好，请先选择业务类型为项目承包的项目，当前页面需要选择业务类型为项目承包方可查看 </el-tag>
    </div>
    <uploadForm v-model="uploadVisible" :globalProject="globalProject" @success="uploadVisible=false;crud.toQuery()"/>
  </div>
</template>

<script setup>
import crudApi from '@/api/project-manage/data-manage/construction-data'
import { ref, watch } from 'vue'

import { businessTypeEnum } from '@enum-ms/contract'
import { constructionDataPM as permission } from '@/page-permission/project'
import { constructionEnum } from '@enum-ms/project'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import udOperation from '@crud/UD.operation'
import pagination from '@crud/Pagination'
import { mapGetters } from '@/store/lib'
import mHeader from './module/header'
import { parseTime } from '@/utils/date'
import uploadForm from './module/upload-form'
import ExportButton from '@comp-common/export-button/index.vue'

const { globalProjectId, globalProject } = mapGetters(['globalProjectId', 'globalProject'])

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const currentRow = ref({})
const currentMonomer = ref({})
const uploadVisible = ref(false)
const { crud, columns, CRUD } = useCRUD(
  {
    title: '施工资料',
    sort: ['id.desc'],
    permission: { ...permission },
    optShow: { ...optShow },
    requiredQuery: ['projectId'],
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.construction-data',
  paginate: true,
  extraHeight: 40
})

watch(
  () => globalProjectId.value,
  (val) => {
    if (val) {
      crud.query.projectId = globalProjectId.value
      crud.toQuery()
    }
  },
  { immediate: true, deep: true }
)

const dataFormat = ref([
  ['createTime', ['parse-time', '{y}-{m}-{d}']],
  ['constructionStageType', ['parse-enum', constructionEnum]]
])

function currentChange(val) {
  currentMonomer.value = val
}

function handleUpload() {
  currentRow.value = {}
  uploadVisible.value = true
}

CRUD.HOOK.beforeRefresh = () => {
  crud.query.projectId = globalProject.value.businessType === businessTypeEnum.INSTALLATION.V ? globalProjectId.value : undefined
  return !!crud.query.projectId
}
// CRUD.HOOK.handleRefresh = (crud, data) => {
//   data.data.content = data.data.content.map(v => {
//     v.edit = false
//     v.originalRemark = v.remark
//     v.editLoading = false
//     return v
//   })
// }
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
</style>
