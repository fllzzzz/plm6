<template>
  <common-drawer
    append-to-body
    ref="drawerRef"
    v-model="visible"
    top="10vh"
    width="600px"
    :before-close="handleClose"
    title="本次绑定列表"
    :wrapper-closable="false"
    size="80%"
    custom-class="current-form"
  >
  <template #titleAfter>
    <el-tag>{{planProcessTypeEnum.VL[currentRow.processType]}}</el-tag>
    <el-tag v-if="currentRow.boolSingleProject">所属项目:{{currentRow.project?projectNameFormatter(currentRow.project):'-'}}</el-tag>
  </template>
  <template #titleRight>
    <common-button size="small" type="primary" v-loading="loading" @click.stop="onSubmit" :disabled="list.length===0">提交（共{{list.length}}条）</common-button>
  </template>
    <template #content>
      <el-form ref="formRef" size="small" label-width="150px">
        <div style="display:flex;">
          <div>
            <!-- <project-cascader v-model="query.projectId" clearable class="filter-item" style="width: 270px;margin-bottom:10px;" placeholder="项目搜索" /> -->
            <div>
              <monomer-select
                ref="monomerSelectRef"
                v-model="query.monomerId"
                style="width: 270px;"
                :default="false"
                clearable
                :project-id="projectId"
                class="filter-item"
                @change="fetchList"
              />
            </div>
            <div style="margin:10px 0;">
              <common-select
                v-model="query.structureClassId"
                :options="structureClassList"
                type="other"
                clearable
                :data-structure="{ key: 'id', label: 'name', value: 'id' }"
                class="filter-item"
                style="width: 270px"
                placeholder="构件类型"
                @change="fetchList"
              />
            </div>
            <div style="margin-bottom:10px;">
              <el-input
                v-model="query.name"
                placeholder="构件名称搜索"
                class="filter-item"
                style="width: 270px;"
                size="small"
                clearable
              />
            </div>
            <div style="margin-bottom:10px;">
              <el-input
                v-model="query.serialNumber"
                placeholder="构件编号"
                class="filter-item"
                style="width: 270px;"
                size="small"
                clearable
              />
            </div>
            <div style="margin-bottom:10px;">
              <el-input
                v-model="query.specification"
                placeholder="构件规格"
                class="filter-item"
                style="width: 270px;"
                size="small"
                clearable
              />
            </div>
            <div style="margin-bottom:10px;">
              <el-input
                v-model="query.material"
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
              :data="list"
              :max-height="maxHeight-80"
              style="width: 100%;"
              class="table-form"
              :data-format="dataFormat"
            >
              <el-table-column label="序号" type="index" align="center" width="50" />
              <el-table-column prop="project" label="项目" align="center" show-overflow-tooltip v-if="!currentRow.boolSingleProject" />
              <el-table-column prop="monomerName" label="单体" align="center" show-overflow-tooltip/>
              <el-table-column prop="serialNumber" label="编号" align="center" show-overflow-tooltip/>
              <el-table-column prop="name" label="构件名称" align="center" show-overflow-tooltip/>
              <el-table-column prop="structureClassName" label="构件类型" align="center" show-overflow-tooltip/>
              <el-table-column prop="specification" label="规格" align="center" show-overflow-tooltip />
              <el-table-column prop="material" label="材质" align="center" show-overflow-tooltip />
              <el-table-column label="操作" align="center">
                <template v-slot="scope">
                  <common-button size="small" type="danger" @click="deleteItem(scope.$index)">移除</common-button>
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
import { bindStructure } from '@/api/plan/technical-data-manage/process'
import { defineProps, defineEmits, ref, watch, inject, nextTick } from 'vue'
import useVisible from '@compos/use-visible'

import { ElMessage } from 'element-plus'
import { planProcessTypeEnum } from '@enum-ms/plan'
import { projectNameFormatter } from '@/utils/project'
import useMaxHeight from '@compos/use-max-height'

import monomerSelect from '@/components-system/plan/monomer-select'

const props = defineProps({
  modelValue: {
    type: Boolean,
    require: true
  },
  showList: {
    type: Object,
    default: () => {}
  },
  projectId: {
    type: [Number, String],
    default: undefined
  },
  monomerId: {
    type: [Number, String],
    default: undefined
  },
  currentRow: {
    type: Object,
    default: () => {}
  }
})

const query = ref({})
const formRef = ref()
const emit = defineEmits(['success', 'update:modelValue', 'delete'])
const { visible, handleClose } = useVisible({ emit, props })
const list = ref([])
const loading = ref(false)
const structureClassList = inject('structureClassList')
const drawerRef = ref()

const dataFormat = ref([
  ['project', 'parse-project']
])

watch(
  () => visible.value,
  (val) => {
    if (val) {
      list.value = JSON.parse(JSON.stringify(props.showList))
      for (const i in query.value) {
        query.value[i] = undefined
      }
      query.value.monomerId = props.monomerId
    }
  },
  { deep: true, immediate: true }
)

const { maxHeight } = useMaxHeight(
  {
    mainBox: '.current-form',
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

function fetchList() {
  let filterVal = JSON.parse(JSON.stringify(props.showList))
  const searchArr = []
  for (const i in query.value) {
    if (query.value[i]) {
      searchArr.push(i)
    }
  }
  for (let i = 0; i < searchArr.length; i++) {
    if (searchArr[i] === 'monomerId' || searchArr[i] === 'structureClassId') {
      filterVal = filterVal.filter(v => v[searchArr[i]] === query.value[searchArr[i]])
    } else {
      filterVal = filterVal.filter(v => v[searchArr[i]].indexOf(query.value[searchArr[i]]) > -1)
    }
  }
  list.value = filterVal
}

function resetSubmit() {
  query.value = {}
  nextTick(() => {
    list.value = JSON.parse(JSON.stringify(props.showList))
  })
}

function deleteItem(index) {
  emit('delete', list.value[index])
  list.value.splice(index, 1)
}

async function onSubmit() {
  loading.value = true
  const submitArr = []
  list.value.forEach(v => {
    submitArr.push({
      monomerId: v.monomerId,
      serialNumber: v.serialNumber
    })
  })
  try {
    await bindStructure({
      processFileId: props.currentRow.id,
      details: submitArr
    })
    ElMessage.success('绑定成功')
    handleSuccess()
  } catch (error) {
    console.log('绑定失败', error)
  } finally {
    loading.value = false
  }
}

</script>
<style lang="scss" scoped>
::v-deep(.el-input-number .el-input__inner) {
  text-align: left;
}
</style>
