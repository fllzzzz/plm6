<template>
  <div id="cls-batch-add">
    <el-dialog
      title="新增科目"
      v-model="dialogVisible"
      width="800px"
      :before-close="handleClose"
      :show-close="false"
      :close-on-click-modal="false"
      top="10vh"
    >
      <div class="flex-rbc heade-operate">
        <common-radio-button v-model="currentLevel" :options="levelOption" type="enum" size="mini" @change="handleLevelChange" />
        <div class="flex-rsc">
          <common-button size="mini" type="success" icon="el-icon-plus" style="padding: 6px" @click="addRow" />
          <common-button :loading="submitLoading" type="primary" size="mini" @click="submit">提 交</common-button>
          <common-button size="mini" @click="handleClose">退 出</common-button>
        </div>
      </div>
      <common-table
        v-if="refreshTable"
        ref="table"
        :data="form.list"
        empty-text="暂无数据"
        :max-height="maxHeight"
        default-expand-all
        :cell-class-name="(data) => wrongCellMask(data, currentRules)"
        style="width: 100%"
      >
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column
          v-if="currentLevel > 1"
          key="parentId"
          prop="parentId"
          :show-overflow-tooltip="true"
          label="上级科目"
          min-width="150"
        >
          <template v-slot="scope">
            <cls-cascader
              v-model="scope.row.parentId"
              :deep="currentLevel - 1"
              show-all-levels
              separator=" > "
              clearable
              placeholder="上级科目"
              :extra-option="{ name: '同上', id: dittos.get('parentId') }"
              size="small"
              style="width: 100%"
            />
          </template>
        </el-table-column>
        <el-table-column key="name" prop="name" :show-overflow-tooltip="true" label="科目名称" min-width="150">
          <template v-slot="scope">
            <el-input v-model.trim="scope.row.name" type="text" clearable placeholder="科目名称" size="small" style="width: 100%" />
          </template>
        </el-table-column>
        <el-table-column key="code" prop="code" :show-overflow-tooltip="true" label="科目代码" width="125">
          <template v-slot="scope">
            <el-input v-model.trim="scope.row.code" type="text" maxlength="3" size="small" placeholder="科目代码" style="width: 100%" />
          </template>
        </el-table-column>
        <el-table-column
          v-if="currentLevel == 1"
          key="basicClass"
          prop="basicClass"
          :show-overflow-tooltip="true"
          label="材料类型"
          width="128"
        >
          <template v-slot="scope">
            <common-select
              v-model="scope.row.basicClass"
              :options="classificationEnum"
              show-extra
              type="enum"
              placeholder="材料类型"
              style="width: 105px"
            />
          </template>
        </el-table-column>
        <el-table-column label="操作" width="70px" align="center" fixed="right">
          <template v-slot="scope">
            <common-button type="danger" icon="el-icon-delete" size="mini" style="padding: 6px" @click.stop="delRow(scope.$index)" />
          </template>
        </el-table-column>
      </common-table>
    </el-dialog>
  </div>
</template>

<script setup>
import { batchAdd } from '@/api/config/classification-manage/classification-config'
import { defineProps, defineEmits, onMounted, watch, ref, reactive, nextTick, computed } from 'vue'
import { classificationEnum } from '@enum-ms/classification'

import useMaxHeight from '@compos/use-max-height'
import useDialogVisible from '@compos/use-dialog-visible'
import useTableValidate, { wrongCellMask } from '@compos/form/use-table-validate'
import clsCascader from '@comp-cls/cascader/index.vue'
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
const currentLevel = ref(1)

// 表单
const form = reactive({
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

const currentRules = computed(() => rules[`LV${currentLevel.value}`])

// 同上的选项与值
const dittos = new Map([
  ['basicClass', -1],
  ['parentId', -1]
])

const { dialogVisible, handleClose } = useDialogVisible(emit, props, () => init())

const { maxHeight } = useMaxHeight(
  {
    mainBox: ['#cls-batch-add', '.el-overlay'],
    extraBox: ['.el-dialog__header', '.heade-operate'],
    wrapperBox: ['.el-dialog__body'],
    extraHeight: '20vh',
    navbar: false
  },
  dialogVisible
)

watch(
  () => props.level,
  (value) => {
    currentLevel.value = value
    handleLevelChange()
  },
  { immediate: true }
)

onMounted(() => {
  rules.LV1 = Object.assign(rules.LV1, rules.common)
  rules.LV2 = Object.assign(rules.LV2, rules.common)
  rules.LV3 = Object.assign(rules.LV3, rules.common)
})

// 初始化表单,默认10条
function init() {
  form.list = []
  for (let i = 0; i < 10; i++) {
    addRow()
  }
}
// 添加行
function addRow() {
  const row = {}
  if (form.list.length > 0) {
    row.basicClass = dittos.get('basicClass')
    row.parentId = dittos.get('parentId')
  }
  form.list.push(row)
}

// 删除行
function delRow(index) {
  form.list.splice(index, 1)
}

// level变更
function handleLevelChange() {
  init()
  refreshTable.value = false
  nextTick(() => {
    refreshTable.value = true
  })
}

// 提交表单
async function submit() {
  try {
    submitLoading.value = true
    const copyList = JSON.parse(JSON.stringify(form.list)) // 深拷贝，避免失败后，数据修改
    const { validResult, dealList } = useTableValidate({ list: copyList, rules: currentRules, dittos })
    form.list = dealList
    if (validResult) {
      // 一级科目
      if (currentLevel.value === 1) {
        let prevAttr
        dealList.forEach((v) => {
          v.parentId = 0
          delete v.verify
          if (v.basicClass === dittos.get('basicClass')) {
            v.basicClass = prevAttr
          } else {
            prevAttr = v.basicClass
          }
        })
      }

      // 二、三级科目
      if (currentLevel.value !== 1) {
        let prevPid
        dealList.forEach((v) => {
          delete v.verify
          if (v.parentId === dittos.get('parentId')) {
            v.parentId = prevPid
          } else {
            prevPid = v.parentId
          }
        })
      }

      await batchAdd(dealList)
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
#cls-batch-add {
  ::v-deep(.el-dialog__body) {
    padding-top: 0px;
  }
  .heade-operate {
    margin-bottom: 10px;
  }
}
</style>
