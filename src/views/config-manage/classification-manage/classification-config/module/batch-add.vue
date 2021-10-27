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
      <div class="flex-rbc">
        <common-radio-button v-model="currentLevel" :options="levelOption" type="enum" size="mini" @change="handleLevelChange" />
        <div class="flex-rsc">
          <common-button size="small" type="success" icon="el-icon-plus" style="padding: 6px" @click="addRow" />
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
        :cell-class-name="handelCellClassName"
        style="width: 100%"
      >
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column v-if="currentLevel > 1" key="pid" prop="pid" :show-overflow-tooltip="true" label="上级科目" min-width="150">
          <template v-slot="scope">
            <mat-cls-cascader
              v-model="scope.row.pid"
              :show-currentLevel="currentLevel - 1"
              show-all-levels
              separator=" > "
              clearable
              placeholder="上级科目"
              :extra-val="{ name: '同上', id: extraVal }"
              size="mini"
              cascader-style="width: 100%;"
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
          key="attribute"
          prop="attribute"
          :show-overflow-tooltip="true"
          label="材料类型"
          width="125"
        >
          <template v-slot="scope">
            <common-select
              v-model="scope.row.attribute"
              :options="classificationEnum"
              show-other
              :extra-val="extraVal"
              type="enum"
              placeholder="材料类型"
              style="width: 100px"
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
import { defineProps, defineEmits, onMounted, watch, ref, reactive, nextTick, computed } from 'vue'
import { classificationEnum } from '@enum-ms/classification'
import { isBlank } from '@data-type/index'
import { obj2arr } from '@/utils/convert/type'
import tableValidate from '@/utils/validate'

import { batchAdd } from '@/api/config/classification-manage/classification-config'
import useMaxHeight from '@compos/use-max-height'
import useCustomizeElDialog from '@compos/use-customize-el-dialog'
import matClsCascader from '@comp-cls/material-cascader/index.vue'
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

// 选择框，option-同上的值
const extraVal = -1
// 表单
const form = reactive({
  list: [] // 添加列表
})

const rules = {
  common: {
    name: [{ required: true, max: 20, message: '不能超过20个字符', trigger: 'blur' }],
    code: [{ max: 3, message: '不能超过3个字符', trigger: 'blur' }]
  },
  LV1: {
    // 添加规则
    attribute: [{ required: true, message: '请选择材料类型', trigger: 'change' }]
  },
  LV2: {
    pid: [{ required: true, message: '请选择上级科目', trigger: 'change' }]
  },
  LV3: {
    pid: [{ required: true, message: '请选择上级科目', trigger: 'change' }]
  }
}

const currentRules = computed(() => rules[`LV${currentLevel.value}`])

const { dialogVisible, handleClose } = useCustomizeElDialog(emit, props)

const maxHeight = useMaxHeight({
  mainBox: ['#cls-batch-add', '.el-overlay'],
  extraBox: ['.el-dialog__header'],
  wrapperBox: ['.el-dialog__body'],
  extraHeight: '30vh',
  navbar: false
}, dialogVisible)

watch(
  () => props.level,
  (value) => {
    currentLevel.value = value
    handleLevelChange()
  },
  { immediate: true }
)

watch(
  () => props.modelValue,
  (flag) => {
    if (!flag) {
      // 关闭重置表单
      nextTick(() => {
        init()
      })
    }
  }
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
    row.attribute = extraVal
    row.pid = extraVal
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
    const list = JSON.parse(JSON.stringify(form.list)) // 深拷贝，避免失败后，数据修改
    if (validate(list)) {
      if (currentLevel.value === 1) {
        let prevAttr
        list.forEach((v) => {
          v.pid = 0
          delete v.verify
          if (v.attribute === extraVal) {
            v.attribute = prevAttr
          } else {
            prevAttr = v.attribute
          }
        })
      }

      if (currentLevel.value !== 1) {
        let prevPid
        list.forEach((v) => {
          delete v.verify
          if (v.pid === extraVal) {
            v.pid = prevPid
          } else {
            prevPid = v.pid
          }
        })
      }

      console.log('list: ', list)
      await batchAdd(list)
      emit('success')
      ElMessage.success('添加成功')
      handleClose()
    }
  } catch (error) {
    console.log('辅材添加', error)
  } finally {
    submitLoading.value = false
  }
}

function validate(list) {
  let flag = true
  let message = '请填写数据'
  if (list && list.length > 0) {
    const _blankRowIndexs = [] // 数据为空的下标
    let _isFirstRow = true // 首行，第一条有数据的记录
    // TODO: 考虑封装
    for (const i in list) {
      const row = list[i]
      delete row.verify // 删除验证字段，避免切换科目级别产生规则混淆，以及进行空行处理

      // ------ 空行处理 start ------
      const rowCopy = JSON.parse(JSON.stringify(row))
      if (rowCopy.attribute === extraVal) {
        delete rowCopy.attribute // 删除同上
      }
      if (rowCopy.pid === extraVal) {
        delete rowCopy.pid // 删除同上
      }
      // delete rowCopy.verify // 删除验证字段
      const rowArr = obj2arr(rowCopy)
      const blankRow = rowArr.every((v) => isBlank(v))
      if (blankRow) {
        _blankRowIndexs.push(i)
        continue
      }
      // ------ 空行处理 end------

      // 首行处理
      if (_isFirstRow) {
        // 处理首行"同上"问题，若首行填写“同上”则视为未填写
        if (row.attribute === extraVal) {
          delete row.attribute
        }
        if (row.pid === extraVal) {
          delete row.pid
        }
        _isFirstRow = false
      }

      row.verify = {}
      for (const rule in currentRules.value) {
        row.verify[rule] = tableValidate(currentRules.value[rule], row[rule])
        if (!row.verify[rule]) {
          flag = false
        }
      }
    }
    // 删除空行
    for (const i in _blankRowIndexs) {
      const index = _blankRowIndexs[i]
      list.splice(index - i, 1)
    }
    if (!flag) {
      form.list = Object.assign([], list)
      message = '请修正表格中标红的信息'
    }
    // 数据为空(全部空行的情况)
    if (list.length === 0) {
      flag = false
    }
  } else {
    flag = false
  }
  if (!flag) {
    ElMessage({ message, type: 'error' })
  }
  return flag
}

// 处理表格变色
function handelCellClassName({ row, column, rowIndex, columnIndex }) {
  let flag = true
  if (row.verify && Object.keys(row.verify) && Object.keys(row.verify).length > 0) {
    if (row.verify[column.property] === false) {
      flag = tableValidate(currentRules.value[column.property], row[column.property])
    }
    if (flag) {
      row.verify[column.property] = true
    }
  }
  return flag ? '' : 'mask-td'
}
</script>

<style lang="scss" scoped>
#cls-batch-add {
  ::v-deep(.el-dialog__body) {
    padding-top: 15px;
  }
}
</style>
