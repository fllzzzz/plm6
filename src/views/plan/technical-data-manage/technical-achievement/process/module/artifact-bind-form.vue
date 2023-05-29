<template>
  <common-drawer
    append-to-body
    ref="drawerRef"
    v-model="visible"
    top="10vh"
    width="600px"
    :before-close="closeConfirm"
    title="绑定构件"
    :wrapper-closable="false"
    size="95%"
    custom-class="bind-form"
  >
    <template #titleRight>
      <common-button size="small" type="success" @click.stop="bindVisible=true" v-permission="permission.detail">查看已绑定构件</common-button>
      <el-badge :value="showList.length" :max="99" style="margin-right:10px;">
        <common-button size="small" type="primary" @click.stop="handleBind">本次绑定构件预览提交</common-button>
      </el-badge>
    </template>
    <template #content>
      <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="100px">
        <el-descriptions class="margin-top" :column="3" border label-width="110">
        <el-descriptions-item label-class-name="desc-label" label="文件名称" :span="currentRow.boolSingleProject?2:3">
          <div>
            <span style="cursor: pointer; color: #409eff;word-break:break-all;" @click.stop="attachmentView(currentRow)">{{currentRow.fileName}}</span>
          </div>
        </el-descriptions-item>
        <el-descriptions-item label-class-name="desc-label" label="所属项目" :span="1" v-if="currentRow.boolSingleProject">
          <el-tooltip placement="top">
            <template #content>
              <template v-if="isNotBlank(currentRow.projectList)">
                <div v-for="item in currentRow.projectList" :key="item.id">{{projectNameFormatter(item)}}</div>
              </template>
              <template v-else>-</template>
            </template>
            <div class="project-div">
              <template v-if="isNotBlank(currentRow.projectList)">
                <template v-if="currentRow.projectList.length===1">
                  <span v-for="item in currentRow.projectList" :key="item.id">{{projectNameFormatter(item)}}</span>
                </template>
                <template v-else>
                  <span v-for="item in currentRow.projectList" :key="item.id">【{{projectNameFormatter(item)}}】</span>
                </template>
              </template>
              <template v-else>-</template>
            </div>
          </el-tooltip>
        </el-descriptions-item>
        <el-descriptions-item label-class-name="desc-label" class-name="content-class" class="" label="文件类型"><div style="width:30%">{{planProcessTypeEnum.VL[currentRow.processType]}}</div></el-descriptions-item>
        <el-descriptions-item label-class-name="desc-label" class-name="content-class" label="文件属性"><div style="width:30%">{{processUseTypeEnum.VL[currentRow.boolSingleProject]}}</div></el-descriptions-item>
        <el-descriptions-item label-class-name="desc-label" class-name="content-class" label="备注">
          <div style="word-break:break-all;">{{currentRow.remark}}</div>
        </el-descriptions-item>
      </el-descriptions>
      <el-descriptions class="margin-top" :column="1" border style="margin:15px 0;">
        <el-descriptions-item label-class-name="desc-label" label="查询范围" label-width="110">
          <div class="head-container" style="margin-bottom:0;">
            <common-radio-button
              v-model="query.boolBindStatus"
              :options="isArtifactBindTypeEnum.ENUM"
              show-option-all
              class="filter-item"
              style="margin-bottom:0;"
              type="enum"
              @change="fetchList"
            />
            <project-cascader :clearable="false" ref="projectRef" v-model="query.projectId" :disabled="currentRow.boolSingleProject" class="filter-item" style="width:350px;margin-bottom:0;" placeholder="项目搜索"  @change="fetchList" />
            <monomer-select
              ref="monomerSelectRef"
              v-model="query.monomerId"
              style="width: 270px;margin-bottom:0;"
              :default="false"
              :project-id="query.projectId"
              clearable
              class="filter-item"
              @change="fetchList"
            />
          </div>
        </el-descriptions-item>
      </el-descriptions>
        <div style="display:flex;">
          <structure-type :query="query" @change="typeChange" :maxHeight="maxHeight-240" />
          <div :style="`padding:10px 15px;background:#f5f5f5;min-height:${maxHeight-240}px;`">
             <div style="display:flex;margin-bottom:10px;">
              <div style="flex:1;padding-right:5px;">
                <common-button class="filter-item" size="small" type="warning" @click.stop="choseAll" style="width:100%;">加入所有</common-button>
              </div>
              <div style="flex:1;text-align:right;padding-left:5px;">
                <common-button
                  class="filter-item"
                  size="small"
                  type="primary"
                  style="width:100%;"
                  @click="confirmSelect"
                  :disabled="!tableSelection?.length"
                >
                  加入所选
                </common-button>
              </div>
            </div>
            <div style="margin-bottom:10px;">
              <el-input
                v-model="tableQuery.name"
                placeholder="构件名称搜索"
                class="filter-item"
                style="width: 270px;"
                size="small"
                clearable
              />
            </div>
            <div style="margin-bottom:10px;">
              <el-input
                v-model="tableQuery.serialNumber"
                placeholder="构件编号"
                class="filter-item"
                style="width: 270px;"
                size="small"
                clearable
              />
            </div>
            <div style="margin-bottom:10px;">
              <el-input
                v-model="tableQuery.specification"
                placeholder="构件规格"
                class="filter-item"
                style="width: 270px;"
                size="small"
                clearable
              />
            </div>
            <div style="margin-bottom:10px;">
              <el-input
                v-model="tableQuery.material"
                placeholder="构件材质"
                class="filter-item"
                style="width: 270px;"
                size="small"
                clearable
              />
            </div>
            <div style="text-align:right;">
              <common-button class="filter-item" size="small" type="success" icon="el-icon-search" @click.stop="fetchList">搜索</common-button>
              <common-button
                class="filter-item"
                size="small"
                type="warning"
                icon="el-icon-refresh"
                @click.stop="resetSubmit"
              >
                重置
              </common-button>
            </div>
            <div style="color:#e6a23c;font-size:13px;margin-top:5px;">*  注意：</div>
            <div style="color:#e6a23c;font-size:13px;">1. 专用文件：只可绑定文件指定项目的构件</div>
          </div>
          <div :style="`flex:1;padding:10px;padding-left:0;background:#f5f5f5;min-height:${maxHeight-240}px`">
            <common-table
              v-loading="tableLoading"
              ref="detailRef"
              border
              :data="list"
              :max-height="maxHeight-240"
              style="width: 100%;"
              class="table-form"
              :data-format="dataFormat"
              @selection-change="handleSelectionChange"
            >
              <el-table-column type="selection" width="55" align="center" :selectable="selectable" />
              <el-table-column label="序号" type="index" align="center" width="50" />
              <el-table-column prop="project" label="项目" align="left" min-width="150" v-if="!currentRow.boolSingleProject" show-overflow-tooltip/>
              <el-table-column prop="monomerName" label="单体" align="left" show-overflow-tooltip/>
              <el-table-column prop="serialNumber" label="编号" align="left" show-overflow-tooltip/>
              <el-table-column prop="name" label="构件名称" align="center" show-overflow-tooltip/>
              <el-table-column prop="specification" label="规格" align="center" show-overflow-tooltip />
              <el-table-column prop="material" label="材质" align="center" show-overflow-tooltip width="80" />
              <el-table-column prop="processFileDTO" label="绑定文件名称" align="left" show-overflow-tooltip>
                <template v-slot="scope">
                  <template v-if="isNotBlank(scope.row.processFileDTO)">
                    <span style="cursor: pointer; color: #409eff" @click="attachmentView(scope.row.processFileDTO)">{{scope.row.processFileDTO.fileName}}</span>
                  </template>
                  <template v-else>-</template>
                </template>
              </el-table-column>
              <el-table-column label="操作" align="center" width="80">
                <template v-slot="scope">
                  <common-button v-if="scope.row.sourceRow?.bindStatus !== artifactBindTypeEnum.YES.V" size="mini" icon="el-icon-plus" type="primary" @click="addItem(scope.row)" />
                  <el-tag type="success" v-else>已绑定</el-tag>
                </template>
              </el-table-column>
            </common-table>
          </div>
        </div>
      </el-form>
      <artifactBindCurrent v-model="currentVisible" :currentRow="currentRow" :showList="showList" @delete="deleteItem" @success="handleSuccess" :projectId="query.projectId" :monomerId="query.monomerId"/>
      <artifactBindDetail v-model="bindVisible" :currentRow="currentRow" @success="emit('success')" />
      <showPdfAndImg v-if="pdfShow" :isVisible="pdfShow" :showType="'attachment'" :id="currentId" @close="pdfShow = false" />
    </template>
  </common-drawer>
</template>

<script setup>
import { getStructureList } from '@/api/plan/technical-data-manage/process'
import { defineProps, defineEmits, ref, watch } from 'vue'
import { ElMessageBox } from 'element-plus'
import useVisible from '@compos/use-visible'

import { isNotBlank } from '@data-type/index'
import { processUseTypeEnum, planProcessTypeEnum } from '@enum-ms/plan'
import { projectNameFormatter } from '@/utils/project'
import { isArtifactBindTypeEnum, artifactBindTypeEnum } from '@enum-ms/plan'
import useUserProjects from '@compos/store/use-user-projects'
import { planProcessListPM as permission } from '@/page-permission/plan'
import useMaxHeight from '@compos/use-max-height'

import artifactBindDetail from './artifact-bind-detail'
import projectCascader from '@comp-base/project-cascader.vue'
import monomerSelect from '@/components-system/plan/monomer-select'
import structureType from './structure-type'
import artifactBindCurrent from './artifact-bind-current'
import showPdfAndImg from '@comp-base/show-pdf-and-img.vue'

const { projects } = useUserProjects()

const props = defineProps({
  modelValue: {
    type: Boolean,
    require: true
  },
  currentRow: {
    type: Object,
    default: () => {}
  }
})

const defaultForm = {
  fileName: undefined
}

const form = ref(JSON.parse(JSON.stringify(defaultForm)))
const query = ref({})
const tableQuery = ref({})
const formRef = ref()
const tableLoading = ref(false)
const list = ref([])
const projectRef = ref()
const detailRef = ref()
const currentVisible = ref(false)
const bindVisible = ref(false)
const pdfShow = ref(false)
const currentId = ref()

const showList = ref([])
const tableSelection = ref([])
const rules = {
  fileName: { required: true, message: '请输入文件命名', trigger: 'blur' }
}
const emit = defineEmits(['success', 'update:modelValue'])
const { visible, handleClose } = useVisible({ emit, props })

watch(
  () => visible.value,
  (val) => {
    if (val) {
      query.value = {}
      query.value.processType = props.currentRow.processType
      query.value.boolBindStatus = isArtifactBindTypeEnum.NO.V
      query.value.projectId = props.currentRow.boolSingleProject ? props.currentRow.project?.id : projects.value[0].id
      fetchList()
      showList.value = []
    }
  },
  { deep: true, immediate: true }
)
const drawerRef = ref()

const { maxHeight } = useMaxHeight(
  {
    mainBox: '.bind-form',
    extraBox: '.el-drawer__header',
    wrapperBox: '.el-drawer__body',
    paginate: false,
    minHeight: 300,
    navbar: false,
    clientHRepMainH: true
  },
  drawerRef
)

function closeConfirm() {
  if (showList.value?.length) {
    ElMessageBox.confirm('您本次操作的绑定记录尚未提交，确认退出？', '提示', { type: 'warning' })
      .then(() => {
        handleClose()
      })
      .catch(() => {
      })
  } else {
    handleClose()
  }
}

function handleSuccess() {
  emit('success')
  handleClose()
}

const dataFormat = ref([
  ['project', 'parse-project']
])

// 是否可选
function selectable(row) {
  return row.sourceRow?.bindStatus !== artifactBindTypeEnum.YES.V
}

// 预览附件
function attachmentView(item) {
  currentId.value = item.attachmentId
  pdfShow.value = true
}

// 获取构件明细
async function fetchList() {
  let _list = []
  if (!query.value.projectId || !props.currentRow.id || !tableQuery.value.structureClassIds) {
    list.value = _list
    return
  }
  tableLoading.value = true
  try {
    const { content = [] } = await getStructureList({ processFileId: props.currentRow.id, ...query.value, ...tableQuery.value })
    content.map((v, index) => {
      v.uuid = v.project?.id + '_' + v.serialNumber
      v.projectId = v.project?.id
    })
    _list = content
  } catch (error) {
    console.log('构件明细', error)
  } finally {
    list.value = _list
    tableLoading.value = false
  }
}

function typeChange(val) {
  const structureClassIds = val.map(v => v.structureClassId) || []
  tableQuery.value.structureClassIds = structureClassIds.join(',')
  fetchList()
}

function resetSubmit() {
  tableQuery.value = {}
  fetchList()
}

function choseAll() {
  list.value.forEach((v) => {
    if (v.bindStatus !== artifactBindTypeEnum.YES.V) {
      detailRef?.value?.toggleRowSelection(v, true)
    }
  })
  tableSelection.value.forEach(v => {
    if (showList.value.findIndex(k => k.uuid === v.uuid) < 0) {
      showList.value.push(v)
    }
  })
}

function confirmSelect() {
  tableSelection.value.forEach(v => {
    if (showList.value.findIndex(k => k.uuid === v.uuid) < 0) {
      showList.value.push(v)
    }
  })
}

function addItem(val) {
  const findVal = list.value.find(v => v.uuid === val.uuid)
  detailRef?.value?.toggleRowSelection(findVal, true)
  tableSelection.value.forEach(v => {
    if (showList.value.findIndex(k => k.uuid === v.uuid) < 0) {
      showList.value.push(v)
    }
  })
}

function handleSelectionChange(val) {
  tableSelection.value = val
}

function deleteItem(val) {
  const findVal = list.value.find(v => v.uuid === val.uuid)
  const findShowIndex = showList.value.findIndex(v => v.uuid === val.uuid)
  if (isNotBlank(findShowIndex)) {
    showList.value.splice(findShowIndex, 1)
  }
  if (findVal) {
    detailRef?.value?.toggleRowSelection(findVal, false)
  }
}

function handleBind() {
  if (!showList.value?.length) {
    return
  }
  currentVisible.value = true
}
</script>
<style lang="scss" scoped>
::v-deep(.el-input-number .el-input__inner) {
  text-align: left;
}
::v-deep(.content-class){
  width:25% !important;
}
::v-deep(.desc-label){
  width:149px !important;
}
.project-div{
  word-break: break-all;
  overflow: hidden;
  text-overflow: ellipsis;
  display: -webkit-box;
  line-clamp: 2;
  -webkit-box-orient: vertical;
  -webkit-line-clamp: 2;
}
</style>
