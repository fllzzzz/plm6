<template>
  <common-drawer
    ref="drawerRef"
    v-model="drawerVisible"
    direction="rtl"
    size="1200px"
    title="项目备料配置"
    :wrapper-closable="false"
    :before-close="handleClose"
    custom-class="project-preparation-range-dlg"
  >
    <template #titleRight>
      <common-button v-show="isEditMode" size="mini" type="primary" @click="previewAndSubmit">保存预览</common-button>
      <common-button v-show="isEditMode" size="mini" type="danger" @click="handleCancelEdit">退出编辑</common-button>
      <common-button v-show="!isEditMode" size="mini" type="primary" @click="isEditMode = true">编辑</common-button>
    </template>
    <template #content>
      <div class="class-unit-config">
        <div class="filter-container">
          <div class="filter-left-box">
            <el-input
              v-model.trim="queryFilter.projectInfo"
              clearable
              size="small"
              placeholder="项目编号/名称"
              class="filter-item"
              style="width: 250px"
            />
            <common-radio-button
              v-model="queryFilter.configureStatus"
              show-option-all
              :options="configureStatusEnum.ENUM"
              type="enum"
              size="small"
              class="filter-item"
            />
            <!-- <common-radio-button
              v-model="queryFilter.preparationRangeType"
              show-option-all
              :options="preparationRangeEnum.ENUM"
              type="enum"
              size="small"
              class="filter-item"
            /> -->
          </div>
        </div>
        <common-table
          ref="table"
          v-loading="loading"
          border
          :data="filterList"
          :max-height="maxHeight"
          :cell-class-name="changedCellMask"
          :highlight-current-row="false"
          row-key="id"
        >
          <el-table-column type="index" label="序号" align="center" width="60" />
          <el-table-column label="项目" align="left">
            <template #default="{ row }">
              <span v-parse-project="{ project: row }" v-empty-text />
            </template>
          </el-table-column>
          <el-table-column label="是否无清单备料" align="center" width="170" prop="withoutList">
            <template #default="{ row }">
              <common-radio
                v-if="isEditMode"
                v-model="row.withoutList"
                :options="whetherEnum.ENUM"
                type="enum"
                :disabled="row.boolStrucPrepared"
              />
              <span v-else v-parse-enum="{ e: whetherEnum, v: row.withoutList }" v-empty />
            </template>
          </el-table-column>
          <el-table-column label="“结构”备料范围" align="center" width="250" prop="strucPreparationRangeType">
            <template #default="{ row }">
              <common-radio
                v-if="isEditMode"
                v-model="row.strucPreparationRangeType"
                :options="preparationRangeEnum.ENUM"
                type="enum"
                :disabled="row.boolStrucPrepared"
              />
              <span v-else v-parse-enum="{ e: preparationRangeEnum, v: row.strucPreparationRangeType }" v-empty />
            </template>
          </el-table-column>
          <el-table-column label="“围护”备料范围" align="center" width="250" prop="enclPreparationRangeType">
            <template #default="{ row }">
              <common-radio
                v-if="isEditMode"
                v-model="row.enclPreparationRangeType"
                :options="preparationRangeEnum.ENUM"
                type="enum"
                :disabled="row.boolEnclPrepared"
              />
              <span v-else v-parse-enum="{ e: preparationRangeEnum, v: row.enclPreparationRangeType }" v-empty />
            </template>
          </el-table-column>
          <el-table-column label="“辅材”备料范围" align="center" width="250" prop="auxPreparationRangeType">
            <template #default="{ row }">
              <!-- 辅材没有区域 -->
              <common-radio
                v-if="isEditMode"
                v-model="row.auxPreparationRangeType"
                :options="preparationRangeEnum.ENUM"
                :unshowVal="[preparationRangeEnum.AREA.V]"
                type="enum"
                :disabled="row.boolAuxPrepared"
              />
              <span v-else v-parse-enum="{ e: preparationRangeEnum, v: row.auxPreparationRangeType }" v-empty />
            </template>
          </el-table-column>
        </common-table>
        <p class="tip form-item-tip">* 项目对应种类进行备料后，项目对应种类的备料范围不可变更</p>
        <preview v-model:visible="previewVisible" :data="modifiedList" @save-success="handleSaveSuccess" />
      </div>
    </template>
  </common-drawer>
</template>

<script setup>
import { getProjectListForRangeInfo } from '@/api/plan/material-preparation/project-preparation'
import { defineProps, defineEmits, ref, computed, provide } from 'vue'
import { preparationRangeEnum } from '@enum-ms/plan'
import { configureStatusEnum, whetherEnum } from '@enum-ms/common'
import { deepClone, isBlank, isNotBlank } from '@/utils/data-type'
import { setSourceInfo } from '@data-type/array'
import { pinyinFuzzyMatching, pinyinForField } from '@/utils/pinyin'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import preview from './preview'
import { judgeItemFieldChange } from '@/utils'
import useTableChange from '@/composables/form/use-table-change'

const emit = defineEmits(['update:visible', 'update'])

const props = defineProps({
  classificationList: {
    type: Array,
    default: () => []
  },
  level: {
    type: Number,
    default: 2
  },
  visible: {
    type: Boolean,
    default: false
  }
})

const queryFilter = ref({
  configureStatus: undefined
  // preparationRangeType: undefined
})

const sourceMap = new Map([
  ['withoutList', 'sourceWithoutList'],
  ['strucPreparationRangeType', 'sourceStrucPreparationRangeType'],
  ['enclPreparationRangeType', 'sourceEnclPreparationRangeType'],
  ['auxPreparationRangeType', 'sourceAuxPreparationRangeType']
])
provide('sourceMap', sourceMap)

const pinyinFields = ['name', 'shortName']

const drawerRef = ref()
// 项目列表
const list = ref([])
// 项目数据源
const sourceList = ref([])
// 预览显示
const previewVisible = ref(false)
// 是否修改
const isEditMode = ref(false)
// 项目加载
const loading = ref(false)

const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible', closeHook: handleCancelEdit })

// 高度
const { maxHeight } = useMaxHeight(
  {
    mainBox: '.project-preparation-range-dlg',
    extraBox: ['.el-drawer__header', '.filter-container', '.tip'],
    wrapperBox: ['.el-drawer__body', '.class-unit-config'],
    navbar: false,
    clientHRepMainH: true,
    minHeight: 300
  },
  () => drawerRef.value.loaded
)

// 修改列表
const modifiedList = computed(() => list.value.filter((v) => judgeItemFieldChange(v, sourceMap)))

// 变更mask
const { changedCellMask } = useTableChange({ fieldMap: sourceMap })

// 过滤后的列表
const filterList = computed(() => {
  return list.value.filter((row) => {
    // 满足条件判断
    let meets = true
    const projectInfo = queryFilter.value.projectInfo
    if (projectInfo) {
      // 编号/项目名称/项目简称
      meets = row.serialNumber.indexOf(projectInfo) > -1 || pinyinFuzzyMatching(row, projectInfo, pinyinFields)
    }
    // 校验
    if (queryFilter.value.configureStatus === configureStatusEnum.UNFINISHED.V) {
      meets =
        meets &&
        isBlank(row.sourceStrucPreparationRangeType) &&
        isBlank(row.sourceEnclPreparationRangeType) &&
        isBlank(row.sourceAuxPreparationRangeType)
      return meets
    }
    if (queryFilter.value.configureStatus === configureStatusEnum.FINISHED.V) {
      meets =
        meets &&
        (isNotBlank(row.sourceStrucPreparationRangeType) ||
          isNotBlank(row.sourceEnclPreparationRangeType) ||
          isNotBlank(row.sourceAuxPreparationRangeType))
      return meets
    }
    return meets
  })
})

// 触发加载
fetchList()

// 加载列表
async function fetchList() {
  try {
    loading.value = true
    const { content } = await getProjectListForRangeInfo()
    list.value = content.map((row) => {
      return setSourceInfo(row, sourceMap)
    })
    pinyinForField(list.value, pinyinFields)
    sourceList.value = deepClone(list.value)
  } catch (error) {
    console.error(error)
  } finally {
    loading.value = false
  }
}

// 提交预览
function previewAndSubmit() {
  previewVisible.value = true
}

// 退出编辑
function handleCancelEdit() {
  isEditMode.value = false
  list.value = sourceList.value
}

// 保存成功
function handleSaveSuccess() {
  isEditMode.value = false
  modifiedList.value.forEach((row) => {
    setSourceInfo(row, sourceMap)
  })
  emit('update')
}
</script>

<style lang="scss" scoped>
$default-cell-mask-color: #52f09840;
::v-deep(.mask-td) {
  .cell {
    &:after {
      background-color: $default-cell-mask-color;
    }
  }
}

.tip {
  font-size: 12px;
}
</style>
