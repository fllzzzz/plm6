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
      <el-descriptions class="margin-top" :column="3" border>
        <el-descriptions-item label-class-name="contractLabel" label="文件名称" :span="2"></el-descriptions-item>
        <el-descriptions-item label-class-name="contractLabel" label="所属项目">
          <template v-if="currentRow.projects && currentRow.projects.length>0">
            <div v-for="item in currentRow.projects" :key="item.id">
              {{item.serialNumber+' '+item.shortName}}
            </div>
          </template>
        </el-descriptions-item>
        <el-descriptions-item label-class-name="contractLabel" label="文件类型"></el-descriptions-item>
        <el-descriptions-item label-class-name="contractLabel" label="文件属性"></el-descriptions-item>
        <el-descriptions-item label-class-name="remark" label="备注">
          <span></span>
        </el-descriptions-item>
      </el-descriptions>
      <el-descriptions class="margin-top" :column="1" border style="margin:15px 0;">
        <el-descriptions-item label-class-name="contractLabel" label="查询范围">
          <div class="head-container" style="margin-bottom:0;">
             <common-radio-button
              v-model="query.useType"
              :options="isArtifactBindTypeEnum.ENUM"
              show-option-all
              class="filter-item"
              style="margin-bottom:0;"
              type="enum"
            />
            <project-cascader v-model="query.projectId" clearable class="filter-item" style="width: 270px;margin-bottom:0;" placeholder="项目搜索" />
            <monomer-select
              ref="monomerSelectRef"
              v-model="form.monomerId"
              style="width: 270px;margin-bottom:0;"
              :project-id="form.projectId"
              class="filter-item"
            />
          </div>
        </el-descriptions-item>
      </el-descriptions>
        <div style="display:flex;">
          <structure-type />
          <div style="padding:0 15px;">
            <div style="margin-bottom:10px;">
              <el-input
                v-model="form.fileName"
                placeholder="构件类型搜索"
                class="filter-item"
                style="width: 270px !important;"
                size="small"
                clearable
              />
            </div>
            <div style="margin-bottom:10px;">
              <el-input
                v-model="form.fileName"
                placeholder="构件类型搜索"
                class="filter-item"
                style="width: 270px;"
                size="small"
                clearable
              />
            </div>
            <div style="margin-bottom:10px;">
              <el-input
                v-model="form.fileName"
                placeholder="构件名称搜索"
                class="filter-item"
                style="width: 270px;"
                size="small"
                clearable
              />
            </div>
            <div style="margin-bottom:10px;">
              <el-input
                v-model="form.fileName"
                placeholder="构件编号"
                class="filter-item"
                style="width: 270px;"
                size="small"
                clearable
              />
            </div>
            <div style="margin-bottom:10px;">
              <el-input
                v-model="form.fileName"
                placeholder="构件规格"
                class="filter-item"
                style="width: 270px;"
                size="small"
                clearable
              />
            </div>
            <div style="margin-bottom:10px;">
              <el-input
                v-model="form.fileName"
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
              ref="detailRef"
              border
              :data="[]"
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
import { defineProps, defineEmits, computed, ref, watch } from 'vue'
import useVisible from '@compos/use-visible'

import { isArtifactBindTypeEnum } from '@enum-ms/plan'
import projectCascader from '@comp-base/project-cascader.vue'
import monomerSelect from '@/components-system/plan/monomer-select'
import structureType from './structure-type'

const props = defineProps({
  currentMonomer: {
    type: Object,
    default: () => {}
  },
  globalProject: {
    type: Object,
    default: () => {}
  },
  dataType: {
    type: [String, Number],
    default: undefined
  },
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
const formRef = ref()
const rules = {
  fileName: { required: true, message: '请输入文件命名', trigger: 'blur' }
}
const emit = defineEmits(['success', 'update:modelValue'])
const { visible, handleClose } = useVisible({ emit, props })

// watch(
//   () => visible.value,
//   (val) => {
//     if (val) {
//       form.value.fileName = undefined
//     }
//   },
//   { deep: true, immediate: true }
// )

const carryParam = computed(() => {
  return {}
  // return props.currentRow.id ? { id: props.currentRow.id, fileName: form.value.fileName } : { projectId: props.globalProject.id, monomerId: props.currentMonomer.id, dataType: props.dataType, fileName: form.value.fileName }
})

function handleSuccess() {
  emit('success')
  handleClose()
}

function fetchList() {

}

function resetSubmit() {

}

</script>
<style lang="scss" scoped>
::v-deep(.el-input-number .el-input__inner) {
  text-align: left;
}
</style>
