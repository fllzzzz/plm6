<template>
  <common-dialog
    append-to-body
    :close-on-click-modal="false"
    :before-close="crud.cancelCU"
    :visible="crud.status.cu > 0"
    :title="crud.query.type === costTypeEnum.ELECTRIC_COST.V ? `${isEdit ? '编辑' : '新增'}电费` : `${isEdit ? '编辑' : '新增'}水费`"
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
        <el-form-item label="月份：" prop="month">
          <el-input-number v-model="form.month" style="width: 270px" placeholder="输入月份" controls-position="right" :min="1" :max="12" />
        </el-form-item>
        <el-form-item label="用量：" prop="usedMete">
          <el-input-number
            v-show-thousand
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
import { costTypeEnum } from '@enum-ms/contract'
import { regForm } from '@compos/use-crud'

const prop = defineProps({
  query: {
    type: Object
  }
})

// 是否是编辑状态
const isEdit = computed(() => {
  return crud.status.edit > 0
})
const formRef = ref()

const defaultForm = {
  id: undefined,
  month: undefined,
  usedMete: undefined,
  totalAmount: undefined
}

const { crud, form, CRUD } = regForm(defaultForm, formRef)

const validateQuantity = (rule, value, callback) => {
  if (!value) {
    callback(new Error('填写数据必须大于0'))
  }
  callback()
}
const rules = {
  month: [{ required: true, message: '请输入月份', trigger: 'blur' }],
  usedMete: [{ required: true, validator: validateQuantity, trigger: 'blur' }],
  totalAmount: [{ required: true, validator: validateQuantity, trigger: 'blur' }]
}

// 编辑之前
CRUD.HOOK.beforeToEdit = (crud, form) => {
  console.log(form, 'form')
}

// 处理刷新数据
CRUD.HOOK.beforeToQuery = async () => {}
// 编辑之前
CRUD.HOOK.beforeToEdit = () => {
  console.log(form)
}

// 提交前
CRUD.HOOK.beforeSubmit = async () => {
  const valid = await formRef.value.validate()
  if (!valid) return false
  form.type = prop.query.type
  form.childType = prop.query.childType
  form.year = prop.query.year
}
</script>

<style lang="scss" scoped></style>
