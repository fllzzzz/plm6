<template>
  <div class="app-container">
    <!--工具栏-->
    <div class="head-container">
      <mHeader @structureClassChange="structureClassChange"/>
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
    <el-table-column v-if="columns.visible('processType')" key="processType" prop="processType" :show-overflow-tooltip="true" label="工艺类型"  width="80" align="center"/>
    <el-table-column v-if="columns.visible('boolSingleProject')" key="boolSingleProject" prop="boolSingleProject" :show-overflow-tooltip="true" width="80" label="文件类型" align="center"/>
    <el-table-column v-if="columns.visible('project')" key="project" prop="project" :show-overflow-tooltip="true" label="所属项目" align="left" min-width="150" />
     <el-table-column v-if="columns.visible('fileName')" key="fileName" prop="fileName" :show-overflow-tooltip="true" label="文件名称" align="left" min-width="120">
      <template v-slot="scope">
        <template v-if="scope.row.attachmentDTO">
          <div style="cursor: pointer; color: #409eff" @dblclick="attachmentView(scope.row.attachmentDTO)">{{ scope.row.fileName }}</div>
        </template>
        <span v-else>-</span>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('remark')" key="remark" prop="remark" :show-overflow-tooltip="true" label="备注"  min-width="120" align="left"/>
    <el-table-column v-if="columns.visible('bindQuantity')" key="bindQuantity" prop="bindQuantity" :show-overflow-tooltip="true" label="绑定构件数量" width="100" align="left"/>
    <el-table-column v-if="columns.visible('fileVersion')" key="fileVersion" prop="fileVersion" :show-overflow-tooltip="true" label="修订版本" width="80" align="center"/>
    <el-table-column v-if="columns.visible('uploadTime')" key="uploadTime" prop="uploadTime" label="上传日期" align="center" width="140" />
    <el-table-column v-if="columns.visible('userName')" key="userName" prop="userName" :show-overflow-tooltip="true" label="上传人" align="center" width="90" />
    <!--编辑与删除-->
    <el-table-column
      v-if="checkPermission([...permission.detail, ...permission.edit,...permission.bind,...permission.del])"
      label="操作"
      width="280px"
      align="center"
      fixed="right"
    >
      <template v-slot="scope">
        <common-button size="mini" @click="openDetail(scope.row)" v-permission="permission.detail">详情</common-button>
        <common-button size="mini" type="primary" @click="openModify(scope.row)" v-permission="permission.edit">修改</common-button>
        <common-button size="mini" type="success" @click="openBind(scope.row)" v-permission="permission.bind">绑定构件</common-button>
        <template v-if="checkPermission(permission.del)">
          <el-tooltip effect="light" v-if="scope.row.sourceRow?.bindQuantity>0" content="已绑定构件，不可删除!" placement="top">
            <div style="display:inline-block;margin-left:6px;">
              <common-button type="danger" :disabled="scope.row.sourceRow?.bindQuantity>0" icon="el-icon-delete" size="mini" />
            </div>
          </el-tooltip>
          <el-popover
            v-else
            v-model:visible="scope.row.pop"
            placement="top"
            width="180"
            trigger="manual"
            @show="onPopoverShow"
            @hide="onPopoverHide"
          >
            <p>{{scope.row.sourceRow?.bindQuantity>0?'已绑定构件，不可删除!':'确定删除？'}}</p>
            <div style="text-align: right; margin: 0" v-if="scope.row.sourceRow?.bindQuantity<=0">
              <common-button size="mini" type="text" @click.stop="cancelDel(scope.row)">取消</common-button>
              <common-button type="primary" size="mini" @click.stop="delClick(scope.row)">确定</common-button>
            </div>
            <template #reference>
              <common-button type="danger" :disabled="scope.row.sourceRow?.bindQuantity>0" icon="el-icon-delete" size="mini" @click.stop="toDelete(scope.row)" />
            </template>
          </el-popover>
        </template>
      </template>
    </el-table-column>
  </common-table>
  <!--分页组件-->
  <pagination />
  <mForm />
  <detail v-model="detailVisible" :currentRow="currentRow" @success="crud.toQuery" />
  <modify-form v-model="modifyVisible" @success="crud.toQuery" :currentRow="currentRow" />
  <artifact-bind-form v-model="bindVisible" @success="crud.toQuery" :currentRow="currentRow" />
  <showPdfAndImg v-if="pdfShow" :isVisible="pdfShow" :showType="'attachment'" :id="currentId" @close="pdfShow = false" />
  </div>
</template>

<script setup>
import crudApi from '@/api/plan/technical-data-manage/process'
import { ref, provide } from 'vue'
import { ElMessage } from 'element-plus'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import checkPermission from '@/utils/system/check-permission'
import { planProcessListPM as permission } from '@/page-permission/plan'
import { processUseTypeEnum, planProcessTypeEnum } from '@enum-ms/plan'

import detail from './module/detail'
import modifyForm from './module/modify-form'
import artifactBindForm from './module/artifact-bind-form'
import mForm from './module/form'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import showPdfAndImg from '@comp-base/show-pdf-and-img.vue'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const currentRow = ref({})

const modifyVisible = ref(false)
const detailVisible = ref(false)
const bindVisible = ref(false)
const pop = ref(false)

const pdfShow = ref(false)
const currentId = ref()
const structureClassList = ref([])
provide('structureClassList', structureClassList)

const dataFormat = ref([
  ['project', 'parse-project'],
  ['uploadTime', ['parse-time', '{y}-{m}-{d} {h}:{i}:{s}']],
  ['processType', ['parse-enum', planProcessTypeEnum]],
  ['boolSingleProject', ['parse-enum', processUseTypeEnum]]
])

const { CRUD, crud, columns } = useCRUD(
  {
    title: '工艺文件',
    sort: ['id.desc'],
    permission: { ...permission },
    optShow: { ...optShow },
    // requiredQuery: ['dataType'],
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.changeFile',
  paginate: true,
  extraHeight: 40
})

function structureClassChange(val) {
  structureClassList.value = val
}

// 预览附件
function attachmentView(item) {
  currentId.value = item.id
  pdfShow.value = true
}

function openDetail(row) {
  currentRow.value = row?.sourceRow
  detailVisible.value = true
}

function openModify(row) {
  currentRow.value = row?.sourceRow
  modifyVisible.value = true
}

function openBind(row) {
  currentRow.value = row?.sourceRow
  bindVisible.value = true
}

async function delClick(row) {
  try {
    await crudApi.del(row.id)
    ElMessage({ type: 'success', message: '删除成功' })
    // 重新查询
    crud.toQuery()
  } catch (err) {
    console.log('删除工艺文件')
  }
}

function toDelete(row) {
  row.pop = true
}
function cancelDel(row) {
  row.pop = false
}

function handleDocumentClick(event) {
  pop.value = false
}

// 打开删除提示窗
function onPopoverShow() {
  setTimeout(() => {
    document.addEventListener('hover', handleDocumentClick, { passive: false })
  }, 0)
}

// 隐藏删除提示窗
function onPopoverHide() {
  document.removeEventListener('hover', handleDocumentClick)
}

// function editRow(row) {
//   currentRow.value = row
//   uploadVisible.value = true
// }

// function currentChange(val) {
//   currentMonomer.value = val
// }

// function handleUpload() {
//   currentRow.value = {}
//   uploadVisible.value = true
// }

// CRUD.HOOK.beforeRefresh = () => {
//   // crud.query.projectId = globalProjectId.value
//   // return crud.query.projectId
// }

CRUD.HOOK.handleRefresh = (crud, data) => {
  data.data.content = data.data.content.map(v => {
    v.project = v.boolSingleProject ? v.projectList[0] : {}
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
</style>
