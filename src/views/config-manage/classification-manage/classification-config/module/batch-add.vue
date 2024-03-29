<template>
  <common-dialog
    title="新增科目"
    v-model="visible"
    width="800px"
    :before-close="handleClose"
    :show-close="true"
    :close-on-click-modal="false"
    custom-class="cls-batch-add"
    top="10vh"
  >
    <template #titleRight>
      <common-button size="mini" type="success" icon="el-icon-plus" @click="addRow(form.list)" />
      <common-button :loading="submitLoading" type="primary" size="mini" @click="submit">提 交</common-button>
      <store-operation type="normal" />
    </template>
    <div class="header-operate">
      <common-radio-button v-model="form.currentLevel" :options="levelOption" type="enum" size="mini" @change="handleLevelChange" />
    </div>
    <el-form ref="formRef" :model="form" :disabled="submitLoading">
      <common-table
        v-if="refreshTable"
        ref="table"
        :data="form.list"
        :show-empty-symbol="false"
        return-source-data
        empty-text="暂无数据"
        :max-height="maxHeight"
        default-expand-all
        :cell-class-name="wrongCellMask"
        row-key="uid"
      >
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column
          v-if="form.currentLevel > 1"
          key="parentId"
          prop="parentId"
          :show-overflow-tooltip="true"
          label="上级科目"
          min-width="150"
        >
          <template #default="{ row, $index }">
            <cls-cascader
              v-model="row.parentId"
              :deep="form.currentLevel - 1"
              show-all-levels
              only-show-current-deep
              separator=" > "
              clearable
              placeholder="上级科目"
              :showExtra="$index !== 0"
              size="small"
              style="width: 100%"
            />
          </template>
        </el-table-column>
        <el-table-column key="name" prop="name" :show-overflow-tooltip="true" label="科目名称" min-width="150">
          <template #default="{ row }">
            <el-input v-model.trim="row.name" type="text" clearable placeholder="科目名称" size="small" style="width: 100%" />
          </template>
        </el-table-column>
        <el-table-column key="code" prop="code" :show-overflow-tooltip="true" label="科目代码" width="125">
          <template #default="{ row }">
            <el-input v-model.trim="row.code" type="text" maxlength="3" size="small" placeholder="科目代码" style="width: 100%" />
          </template>
        </el-table-column>
        <el-table-column
          v-if="form.currentLevel == 1"
          key="basicClass"
          prop="basicClass"
          :show-overflow-tooltip="true"
          label="材料类型"
          width="128"
        >
          <template #default="{ row, $index }">
            <common-select
              :key="Math.random()"
              v-model="row.basicClass"
              :options="rawMatClsEnum.ENUM"
              :show-extra="$index !== 0"
              type="enum"
              placeholder="材料类型"
              style="width: 105px"
            />
          </template>
        </el-table-column>
        <el-table-column label="操作" width="70px" align="center" fixed="right">
          <template #default="{ $index }">
            <common-button
              type="danger"
              icon="el-icon-delete"
              size="mini"
              style="padding: 6px"
              @click.stop="removeRow(form.list, $index)"
            />
          </template>
        </el-table-column>
      </common-table>
    </el-form>
  </common-dialog>
</template>

<script setup>
import { batchAdd } from '@/api/config/classification-manage/classification-config'
import { defineProps, defineEmits, onMounted, watch, ref, reactive, nextTick, computed } from 'vue'
import { rawMatClsEnum } from '@enum-ms/classification'
import { deepClone } from '@data-type/index'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import useTableOperate from '@compos/form/use-table-operate'
import useTableValidate from '@compos/form/use-table-validate'
import useAddFormLocalStorage from '@compos/form/use-add-form-local-storage'
import clsCascader from '@comp-cls/cascader/index.vue'
import StoreOperation from '@crud/STORE.operation.vue'
import { ElMessage } from 'element-plus'

// 等级枚举
const levelOption = [
  { K: 1, V: 1, L: '一级' },
  { K: 2, V: 2, L: '二级' },
  { K: 3, V: 3, L: '三级' }
]

const emit = defineEmits(['success', 'update:modelValue'])

const props = defineProps({
  modelValue: {
    type: Boolean,
    require: true
  },
  level: {
    type: Number,
    default: 1
  }
})

// 提交loading
const submitLoading = ref(false)
// 刷新表格
const refreshTable = ref(true)
// 当前等级
// const currentLevel = ref(1)

// 表单
const form = reactive({
  currentLevel: 1, // 当前等级
  list: [] // 添加列表
})

const rules = {
  common: {
    name: [{ required: true, max: 20, message: '不能超过20个字符', trigger: 'blur' }],
    code: [{ required: true, max: 3, message: '不能超过3个字符', trigger: 'blur' }]
  },
  LV1: {
    // 添加规则
    basicClass: [{ required: true, message: '请选择材料类型', trigger: 'change' }]
  },
  LV2: {
    parentId: [{ required: true, message: '请选择上级科目', trigger: 'change' }]
  },
  LV3: {
    parentId: [{ required: true, message: '请选择上级科目', trigger: 'change' }]
  }
}

const currentRules = computed(() => rules[`LV${form.currentLevel}`])

// 同上的选项与值
const ditto = new Map([
  ['basicClass', -1],
  ['parentId', -1]
])

const { visible, handleClose } = useVisible({ emit, props })
const { init, addRow, removeRow } = useTableOperate({}, 10, ditto)
const { tableValidate, cleanUpData, wrongCellMask } = useTableValidate({ rules: currentRules, ditto })
const { ADD_FORM, clearFormStorage } = useAddFormLocalStorage('CLASSIFICATION_CONFIG', form, visible)

ADD_FORM.init = () => init(form.list)

const { maxHeight } = useMaxHeight(
  {
    mainBox: '.cls-batch-add',
    extraBox: ['.el-dialog__header', '.header-operate'],
    wrapperBox: ['.el-dialog__body'],
    clientHRepMainH: true,
    navbar: false
  },
  visible
)

watch(
  () => props.level,
  (value) => {
    form.currentLevel = value
    handleLevelChange()
  },
  { immediate: true }
)

onMounted(() => {
  rules.LV1 = Object.assign(rules.LV1, rules.common)
  rules.LV2 = Object.assign(rules.LV2, rules.common)
  rules.LV3 = Object.assign(rules.LV3, rules.common)
})

// level变更
function handleLevelChange() {
  init(form.list)
  refreshTable.value = false
  nextTick(() => {
    refreshTable.value = true
  })
}

// 提交表单
async function submit() {
  try {
    submitLoading.value = true
    const { validResult, dealList } = tableValidate(form.list)
    form.list = dealList
    if (validResult) {
      // 清除无用数据
      const _list = cleanUpData(deepClone(dealList))
      // 数据格式化
      await batchAdd(_list)
      // 清除本地缓存
      clearFormStorage()
      emit('success')
      ElMessage.success('添加成功')
      handleClose()
    }
  } catch (error) {
    console.log('科目添加', error)
  } finally {
    submitLoading.value = false
  }
}
</script>

<style lang="scss" scoped>
.cls-batch-add {
  ::v-deep(.el-dialog__body) {
    padding-top: 0px;
  }
  .header-operate {
    margin-bottom: 15px;
  }
}
</style>
