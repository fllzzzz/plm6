<template>
  <common-drawer
    append-to-body
    :close-on-click-modal="false"
    :before-close="crud.cancelCU"
    :visible="crud.status.cu > 0"
    :title="crud.status.title"
    :wrapper-closable="false"
    size="700px"
  >
    <template #titleRight>
      <common-button :loading="crud.status.cu === 2" type="primary" size="mini" @click="crud.submitCU">确认</common-button>
    </template>
    <template #content>
      <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="120px">
        <el-form-item label="产品类型" prop="productType">
          <common-radio-button
            v-model="form.productType"
            :options="typeInfo"
            :type="'other'"
            :dataStructure="typeProp"
            class="filter-item"
          />
        </el-form-item>
        <el-form-item label="制造方式" prop="type">
          <el-radio-group v-model="form.type" size="small" class="filter-item"  @change="crud.toQuery">
            <el-radio-button
              v-for="item in manufactureTypeEnum.ENUM"
              :key="item.V"
              :label="item.V"
            >
              {{ item.L }}
            </el-radio-button>
          </el-radio-group>
        </el-form-item>
        <el-form-item label="区域名称" prop="name">
          <el-input
            v-model="form.name"
            type="text"
            placeholder="请填写区域名称"
            style="width: 270px;"
          />
        </el-form-item>
        <el-form-item label="轴线/标高" prop="axis">
          <el-input
            v-model="form.axis"
            type="text"
            placeholder="请填写轴线或标高"
            style="width: 270px;"
          />
        </el-form-item>
        <el-form-item label="完成时间" prop="date">
          <el-date-picker
            v-model="form.date"
            type="date"
            value-format="x"
            placeholder="选择完成日期"
            style="width:270px"
            :disabledDate="dateOptionFn"
          />
        </el-form-item>
        <el-form-item label="排序" prop="sort">
          <el-input-number
            v-model.number="form.sort"
            :min="1"
            :max="999"
            :step="1"
            placeholder="请填写排序值"
            controls-position="right"
            style="width: 270px;"
          />
        </el-form-item>
        <el-form-item label="备注" prop="remark">
          <el-input
            v-model="form.remark"
            type="textarea"
            :autosize="{ minRows: 4, maxRows: 6}"
            placeholder="请填写备注"
            style="width: 320px;"
          />
        </el-form-item>
      </el-form>
    </template>
  </common-drawer>
</template>

<script setup>
import { ref, defineProps } from 'vue'
import { regForm } from '@compos/use-crud'
import { manufactureTypeEnum } from '@enum-ms/plan'
import { ElRadioGroup } from 'element-plus'

const formRef = ref()
const props = defineProps({
  projectId: {
    type: [Number, String],
    default: undefined
  },
  typeInfo: {
    type: Array,
    default: () => []
  },
  globalProject: {
    type: Object,
    default: () => {}
  },
  currentMonomer: {
    type: Object,
    default: () => {}
  }
})
const typeProp = { key: 'no', label: 'name', value: 'no' }
const defaultForm = {
  id: undefined,
  monomerId: undefined,
  name: '',
  axis: '',
  type: manufactureTypeEnum.HOMEMADE.V,
  productType: undefined,
  date: undefined,
  sort: 1,
  remark: ''
}
const { CRUD, crud, form } = regForm(defaultForm, formRef)

const checkOtherDate = (rule, value, callback) => {
  if (!value) {
    callback(new Error('请选择完成时间'))
  } else {
    if (crud.form.productType && props.typeInfo && props.typeInfo.length > 0) {
      const val = props.typeInfo.find(v => v.no === crud.form.productType)
      if (value > val.date) {
        callback(new Error('不能超过产品类型完成时间'))
      } else {
        callback()
      }
    }
  }
}
const rules = {
  productType: [
    { required: true, message: '请选择产品类型', trigger: 'change' }
  ],
  type: [
    { required: true, message: '请选择制造方式', trigger: 'change' }
  ],
  date: [
    { required: true, validator: checkOtherDate, trigger: 'change' }
  ],
  sort: [
    { required: true, message: '请填写排序值', trigger: 'blur', type: 'number' }
  ],
  name: [
    { required: true, message: '请填写区域名称', trigger: 'blur' },
    { min: 1, max: 32, message: '长度在 1 到 32 个字符', trigger: 'blur' }
  ],
  axis: [
    { required: true, message: '请填写轴线或标高', trigger: 'blur' },
    { min: 1, max: 32, message: '长度在 1 到 32 个字符', trigger: 'blur' }
  ],
  remark: [{ max: 500, message: '不能超过 500 个字符', trigger: 'blur' }]
}

function dateOptionFn(time) {
  if (crud.form.productType && props.typeInfo && props.typeInfo.length > 0) {
    const val = props.typeInfo.find(v => v.no === crud.form.productType)
    return time.getTime() - 8.64e6 > val.date || (props.currentMonomer.startDate ? time.getTime() < props.currentMonomer.startDate : time.getTime() < props.globalProject.startDate)
  } else {
    return time.getTime() < (props.currentMonomer.startDate ? time.getTime() < props.currentMonomer.startDate : time.getTime() < props.globalProject.startDate)
  }
}

CRUD.HOOK.afterToAdd = (crud, form) => {
  crud.form.productType = crud.query.productType
}

CRUD.HOOK.beforeSubmit = (crud, form) => {
  crud.form.monomerId = crud.query.monomerId
  return !!crud.form.monomerId
}
</script>
<style scoped>
::v-deep(.el-input-number .el-input__inner) {
  text-align: left;
}
</style>

