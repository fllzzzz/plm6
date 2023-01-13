<template>
  <common-dialog
    append-to-body
    :close-on-click-modal="false"
    :before-close="crud.cancelCU"
    :visible="crud.status.cu > 0"
    :title="`${isEdit ? '编辑' : '新增'} ${gasType} 气体统计`"
    :show-close="false"
    width="25%"
    top="10vh"
  >
    <template #titleRight>
      <span style="float: right">
        <common-button :loading="crud.status.cu === CRUD.STATUS.PROCESSING" size="mini" type="primary" @click="crud.submitCU">
          提 交
        </common-button>
        <common-button size="mini" @click="crud.cancelCU">关 闭</common-button>
      </span>
    </template>
    <div class="form">
      <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="140px" class="demo-form">
        <el-form-item label="年份：" prop="year">
          <el-date-picker
            v-model="form.year"
            type="year"
            size="small"
            format="YYYY"
            value-format="YYYY"
            placeholder="选择日期"
            style="width: 270px"
          />
        </el-form-item>
        <el-form-item label="月份：" prop="month">
          <el-input-number
            v-model="form.month"
            style="width: 270px"
            placeholder="输入月份"
            controls-position="right"
            :step="1"
            :min="1"
            :max="12"
          />
        </el-form-item>
        <el-form-item :label="`用量（${form.accountingUnit ? form.accountingUnit : ''}）：`" prop="usedMete">
          <el-input-number
            v-model="form.usedMete"
            style="width: 270px"
            placeholder="输入用量"
            controls-position="right"
            :min="0"
            :max="9999999999"
          />
        </el-form-item>
        <el-form-item label="费用总额（元）：" prop="totalAmount">
          <el-input-number
            v-show-thousand
            v-model="form.totalAmount"
            style="width: 270px"
            placeholder="输入费用总额"
            controls-position="right"
            :min="0"
            :max="9999999999"
          />
        </el-form-item>
      </el-form>
    </div>
  </common-dialog>
</template>

<script setup>
import { ref, defineProps, computed } from 'vue'
import { regForm } from '@compos/use-crud'
import { ElMessage } from 'element-plus'

const prop = defineProps({
  query: {
    type: Object
  },
  accountingUnit: {
    type: String,
    default: ''
  },
  gasType: {
    type: String,
    default: ''
  }
})

const formRef = ref()

const defaultForm = {
  id: undefined,
  year: undefined,
  month: undefined,
  usedMete: undefined,
  totalAmount: undefined
}

const { crud, form, CRUD } = regForm(defaultForm, formRef)

// 是否是编辑状态
const isEdit = computed(() => {
  return crud.status.edit > 0
})

const rules = {
  month: [{ required: true, message: '请输入月份', trigger: 'blur' }],
  usedMete: [{ required: true, message: '请输入用量', trigger: 'blur' }],
  totalAmount: [{ required: true, message: '请输入费用总额', trigger: 'blur' }]
}

CRUD.HOOK.beforeToAdd = (crud, form) => {
  form.year = crud.query.year
  form.accountingUnit = prop.accountingUnit
  if (!form.accountingUnit) {
    ElMessage.error('请到科目管理里面配置该' + prop.gasType + '气体的计量单位')
    return false
  }
}
// 编辑之前
CRUD.HOOK.beforeToEdit = (crud, form) => {}

// 处理刷新数据
CRUD.HOOK.beforeToQuery = async () => {}
// 编辑之前
CRUD.HOOK.beforeToEdit = () => {
  form.year = prop.query.year
}

// 提交前
CRUD.HOOK.beforeSubmit = async () => {
  form.classifyId = crud.query.classifyId
  form.year = prop.query.year
  form.accountingUnit = prop.accountingUnit
}
</script>

<style lang="scss" scoped></style>
