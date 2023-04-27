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
    custom-class="contract-change1"
  >
    <template #content>
      <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="100px">
        <el-descriptions class="margin-top" :column="3" border label-width="110">
        <el-descriptions-item label-class-name="fileName" label="文件名称" :span="2">{{currentRow.fileName}}</el-descriptions-item>
        <el-descriptions-item label-class-name="project" label="所属项目" :span="1" v-if="currentRow.boolSingleProject">
          {{currentRow.project?projectNameFormatter(currentRow.project):'-'}}
        </el-descriptions-item>
        <el-descriptions-item label-class-name="processType" label="文件类型">{{planProcessTypeEnum.VL[currentRow.processType]}}</el-descriptions-item>
        <el-descriptions-item label-class-name="boolSingleProject" label="文件属性">{{processUseTypeEnum.VL[currentRow.boolSingleProject]}}</el-descriptions-item>
        <el-descriptions-item label-class-name="remark" label="备注">
          {{currentRow.remark}}
        </el-descriptions-item>
      </el-descriptions>
      <el-descriptions class="margin-top" :column="1" border style="margin:15px 0;">
        <el-descriptions-item label-class-name="contractLabel" label="查询范围">
          <div class="head-container" style="margin-bottom:0;">
             <common-radio-button
              v-model="tableQuery.boolBindStatus"
              :options="isArtifactBindTypeEnum.ENUM"
              show-option-all
              class="filter-item"
              style="margin-bottom:0;"
              type="enum"
            />
            <project-cascader :clearable="false" ref="projectRef" v-model="query.projectId" class="filter-item" style="width: 270px;margin-bottom:0;" placeholder="项目搜索" />
            <monomer-select
              ref="monomerSelectRef"
              v-model="query.monomerId"
              style="width: 270px;margin-bottom:0;"
              :default="false"
              :project-id="query.projectId"
              clearable
              class="filter-item"
            />
          </div>
        </el-descriptions-item>
      </el-descriptions>
        <div style="display:flex;">
          <structure-type :query="query" @change="typeChange"/>
          <div style="padding:0 15px;">
            <div style="margin-bottom:10px;">
              <el-input
                v-model="tableQuery.name"
                placeholder="构件名称搜索"
                class="filter-item"
                style="width: 200px;"
                size="small"
                clearable
              />
            </div>
            <div style="margin-bottom:10px;">
              <el-input
                v-model="tableQuery.serialNumber"
                placeholder="构件编号"
                class="filter-item"
                style="width: 200px;"
                size="small"
                clearable
              />
            </div>
            <div style="margin-bottom:10px;">
              <el-input
                v-model="tableQuery.specification"
                placeholder="构件规格"
                class="filter-item"
                style="width: 200px;"
                size="small"
                clearable
              />
            </div>
            <div style="margin-bottom:10px;">
              <el-input
                v-model="tableQuery.material"
                placeholder="构件材质"
                class="filter-item"
                style="width: 200px;"
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
              :max-height="300"
              style="width: 100%;"
              class="table-form"
              return-source-data
            >
              <el-table-column label="序号" type="index" align="center" width="50" />
              <el-table-column prop="depositBank" label="项目" align="center" show-overflow-tooltip/>
              <el-table-column prop="account" label="单体" align="center" show-overflow-tooltip/>
              <el-table-column prop="account" label="编号" align="center" show-overflow-tooltip/>
              <el-table-column prop="depositBank" label="规格" align="center" show-overflow-tooltip />
              <el-table-column prop="depositBank" label="材质" align="center" show-overflow-tooltip />
              <el-table-column label="操作" align="center">
                <template v-slot="scope">
                  <common-button size="small" class="el-icon-plus" type="danger" />
                </template>
              </el-table-column>
            </common-table>
          </div>
        </div>
      </el-form>
    </template>
  </common-drawer>
</template>

<script setup>
import { getStructureList } from '@/api/plan/technical-data-manage/process'
import { defineProps, defineEmits, computed, ref, watch } from 'vue'
import useVisible from '@compos/use-visible'

import { processUseTypeEnum, planProcessTypeEnum } from '@enum-ms/plan'
import { projectNameFormatter } from '@/utils/project'
import { isArtifactBindTypeEnum } from '@enum-ms/plan'
import useUserProjects from '@compos/store/use-user-projects'

import projectCascader from '@comp-base/project-cascader.vue'
import monomerSelect from '@/components-system/plan/monomer-select'
import structureType from './structure-type'

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

const rules = {
  fileName: { required: true, message: '请输入文件命名', trigger: 'blur' }
}
const emit = defineEmits(['success', 'update:modelValue'])
const { visible, handleClose } = useVisible({ emit, props })

watch(
  () => visible.value,
  (val) => {
    if (val) {
      query.value.projectId = props.currentRow.boolSingleProject ? props.currentRow.project?.id : projects.value[0].id
    }
  },
  { deep: true, immediate: true }
)

function handleSuccess() {
  emit('success')
  handleClose()
}

// 获取构件明细
async function fetchList() {
  let _list = []
  if (!query.value.projectId) {
    return
  }
  tableLoading.value = true
  try {
    const { content = [] } = await getStructureList({ ...query, ...tableQuery,processFileId: })
    _list = content
  } catch (error) {
    console.log('收货明细', error)
  } finally {
    list.value = _list
    tableLoading.value = false
  }
}

function typeChange(val) {
  const structureClassIds = val.map(v => v.structureClassId) || []
  tableQuery.value.structureClassIds = structureClassIds.join(',')
}

function resetSubmit() {

}

</script>
<style lang="scss" scoped>
::v-deep(.el-input-number .el-input__inner) {
  text-align: left;
}
</style>
