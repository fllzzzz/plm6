<template>
  <common-drawer
    append-to-body
    ref="drawerRef"
    v-model="visible"
    top="10vh"
    width="600px"
    :before-close="handleClose"
    title="绑定构件"
    :wrapper-closable="false"
    size="95%"
    custom-class="bind-form"
  >
    <template #titleRight>
      <common-button size="small" type="success" @click.stop="bindVisible=true" v-permission="permission.bind">已绑定构件</common-button>
      <el-badge :value="showList.length" :max="99" style="margin-right:10px;">
        <common-button size="small" type="primary" @click.stop="handleBind">本次绑定构件预览提交</common-button>
      </el-badge>
    </template>
    <template #content>
      <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="100px">
        <el-descriptions class="margin-top" :column="3" border label-width="110">
        <el-descriptions-item label-class-name="fileName" label="文件名称" :span="currentRow.boolSingleProject?2:3">
          <div>
            <span>{{currentRow.fileName}}</span>
            <common-button style="float:right;" type="primary">查看文件</common-button>
          </div>
        </el-descriptions-item>
        <el-descriptions-item label-class-name="project" label="所属项目" :span="1" v-if="currentRow.boolSingleProject">
          {{currentRow.project?projectNameFormatter(currentRow.project):'-'}}
        </el-descriptions-item>
        <el-descriptions-item class-name="content-class" class="" label="文件类型"><div style="width:30%">{{planProcessTypeEnum.VL[currentRow.processType]}}</div></el-descriptions-item>
        <el-descriptions-item class-name="content-class" label="文件属性"><div style="width:30%">{{processUseTypeEnum.VL[currentRow.boolSingleProject]}}</div></el-descriptions-item>
        <el-descriptions-item class-name="content-class" label="备注">
          <div style="word-break:break-all;">{{currentRow.remark}}</div>
        </el-descriptions-item>
      </el-descriptions>
      <el-descriptions class="margin-top" :column="1" border style="margin:15px 0;">
        <el-descriptions-item label-class-name="contractLabel" label="查询范围">
          <div class="head-container" style="margin-bottom:0;">
            <project-cascader :clearable="false" ref="projectRef" v-model="query.projectId" :disabled="currentRow.boolSingleProject" class="filter-item" style="width: 270px;margin-bottom:0;" placeholder="项目搜索"  @change="fetchList" />
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
          <structure-type :query="query" @change="typeChange" :maxHeight="maxHeight-220" />
          <div style="padding:0 15px;">
             <div style="display:flex;margin-bottom:5px;">
              <div style="flex:1;">
                <common-button class="filter-item" size="small" type="warning" @click.stop="choseAll">加入所有</common-button>
              </div>
              <div style="flex:1;text-align:right;">
                <common-button
                  class="filter-item"
                  size="small"
                  type="primary"
                >
                  加入所选
                </common-button>
              </div>
            </div>
            <div style="margin-bottom:10px;">
              <common-radio-button
                v-model="tableQuery.boolBindStatus"
                :options="isArtifactBindTypeEnum.ENUM"
                show-option-all
                class="filter-item"
                style="margin-bottom:0;"
                type="enum"
                @change="fetchList"
              />
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
          </div>
          <div style="flex:1;padding-left:10px;">
            <common-table
              v-loading="tableLoading"
              ref="detailRef"
              border
              :data="list"
              :max-height="maxHeight-220"
              style="width: 100%;"
              class="table-form"
              :data-format="dataFormat"
              @selection-change="handleSelectionChange"
            >
              <el-table-column type="selection" width="55" align="center" />
              <el-table-column label="序号" type="index" align="center" width="50" />
              <el-table-column prop="project" label="项目" align="center" v-if="!currentRow.boolSingleProject" show-overflow-tooltip/>
              <el-table-column prop="monomerName" label="单体" align="center" show-overflow-tooltip/>
              <el-table-column prop="serialNumber" label="编号" align="center" show-overflow-tooltip/>
              <el-table-column prop="name" label="构件名称" align="center" show-overflow-tooltip/>
              <el-table-column prop="specification" label="规格" align="center" show-overflow-tooltip />
              <el-table-column prop="material" label="材质" align="center" show-overflow-tooltip />
              <el-table-column prop="boolBindStatus" label="状态" align="center" show-overflow-tooltip />
            </common-table>
          </div>
        </div>
      </el-form>
      <artifactBindCurrent v-model="currentVisible" :currentRow="currentRow" :showList="showList" @delete="deleteItem" @success="handleSuccess" :projectId="query.projectId" :monomerId="query.monomerId"/>
      <artifactBindDetail v-model="bindVisible" :currentRow="currentRow" @success="handleSuccess" />
    </template>
  </common-drawer>
</template>

<script setup>
import { getStructureList } from '@/api/plan/technical-data-manage/process'
import { defineProps, defineEmits, ref, watch } from 'vue'
import useVisible from '@compos/use-visible'

import { processUseTypeEnum, planProcessTypeEnum } from '@enum-ms/plan'
import { projectNameFormatter } from '@/utils/project'
import { isArtifactBindTypeEnum } from '@enum-ms/plan'
import useUserProjects from '@compos/store/use-user-projects'
import { planProcessListPM as permission } from '@/page-permission/plan'
import useMaxHeight from '@compos/use-max-height'

import artifactBindDetail from './artifact-bind-detail'
import projectCascader from '@comp-base/project-cascader.vue'
import monomerSelect from '@/components-system/plan/monomer-select'
import structureType from './structure-type'
import artifactBindCurrent from './artifact-bind-current'

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

const showList = ref([])
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
      query.value.projectId = props.currentRow.boolSingleProject ? props.currentRow.project?.id : projects.value[0].id
      fetchList()
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

function handleSuccess() {
  emit('success')
  handleClose()
}

const dataFormat = ref([
  ['project', 'parse-project'],
  ['boolBindStatus', ['parse-enum', isArtifactBindTypeEnum]]
])

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
      v.uuid = '_' + index
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
    detailRef?.value?.toggleRowSelection(v, true)
  })
}

// function addItem(val) {
//   const findVal = list.value.find(v => v.uuid === val.uuid)
//   detailRef?.value?.toggleRowSelection(findVal, true)
// }

function handleSelectionChange(val) {
  showList.value = val
}

function deleteItem(val) {
  const findVal = list.value.find(v => v.uuid === val.uuid)
  detailRef?.value?.toggleRowSelection(findVal, false)
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
::v-deep(.el-descriptions__label.el-descriptions__cell.is-bordered-label){
  width:110px;
}
::v-deep(.content-class){
  width:25% !important;
}
</style>
