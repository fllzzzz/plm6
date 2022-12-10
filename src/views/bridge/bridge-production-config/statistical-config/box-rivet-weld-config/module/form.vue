<template>
  <common-dialog
    append-to-body
    :close-on-click-modal="false"
    :before-close="crud.cancelCU"
    :visible="crud.status.cu > 0"
    :title="crud.status.title"
    width="500px"
  >
    <template #titleRight>
      <common-button :loading="crud.status.cu === 2" type="primary" size="mini" @click="crud.submitCU">确认</common-button>
    </template>
    <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="110px" label-position="right">
      <el-form-item label="分段类型" prop="classificationId">
        <common-select
          v-model="form.classificationId"
          :options="artifactTypeList"
          type="other"
          class="input-underline"
          :dataStructure="{ key: 'id', label: 'name', value: 'id' }"
          placeholder="分段类型"
          :disabled="isEdit"
          style="width: 100%"
          @change="handleClassificationChange"
        />
      </el-form-item>
      <el-form-item label="截面前缀" prop="specPrefixStr">
        <el-tag v-if="form.classificationId">{{ artifactTypeListObj[form.classificationId]?.specPrefixStr }}</el-tag>
        <el-tag v-else>请选择分段类型</el-tag>
      </el-form-item>
      <el-form-item label="数值" prop="numerical">
        <common-input-number
          v-model="form.minNumerical"
          :step="1"
          :min="0"
          :max="form.maxNumerical"
          :precision="2"
          clearable
          :controls="false"
          size="mini"
          class="input-underline"
          placeholder="最小数值"
          style="width: 45%"
        />
        <span> ~ </span>
        <common-input-number
          v-model="form.maxNumerical"
          :step="1"
          :min="form.minNumerical"
          :precision="2"
          class="input-underline"
          :controls="false"
          size="mini"
          clearable
          placeholder="最大数值"
          style="width: 45%"
        />
      </el-form-item>
      <el-tag
        v-if="isNotBlank(typeProcessObj)"
        style="width: 100%; text-align: center; margin-bottom: 15px; margin-top: 10px"
        type="info"
        size="small"
      >
        工序单价（元）
      </el-tag>
      <template v-for="item in typeProcessObj" :key="item">
        <el-form-item :label="processListObj[item.processId].name">
          <common-select
            v-model="form.processObj[item.processId].wageQuotaType"
            :options="wageQuotaTypeEnum.ENUM"
            type="enum"
            clearable
            size="mini"
            class="input-underline"
            placeholder="计量方式"
            style="width: 40%; margin-right: 10px"
          />
          <common-input-number
            v-model="form.processObj[item.processId].price"
            :step="1"
            :min="0"
            :precision="2"
            clearable
            class="input-underline"
            :controls="false"
            placeholder="单价"
            size="mini"
            style="width: 30%"
          />
          <span v-if="form.processObj[item.processId]?.wageQuotaType">
            {{ wageQuotaTypeEnum.V[form.processObj[item.processId].wageQuotaType].unit }}
          </span>
        </el-form-item>
      </template>
    </el-form>
  </common-dialog>
</template>

<script setup>
import { ref, inject, computed } from 'vue'
import { wageQuotaTypeEnum } from '@enum-ms/mes'
import { regForm } from '@compos/use-crud'
import { isNotBlank } from '@/utils/data-type'
import { obj2arr } from '@/utils/convert/type'

const formRef = ref()
const defaultForm = {
  id: undefined,
  processObj: {},
  boxTypeId: undefined,
  structureProcessPriceList: []
}

const processListObj = inject('processListObj')
const artifactTypeList = inject('artifactTypeList')
const artifactTypeListObj = inject('artifactTypeListObj')

const { crud, CRUD, form } = regForm(defaultForm, formRef)

const typeProcessObj = computed(() => artifactTypeListObj.value[form.classificationId]?.typeProcessObj || {})
// 是否是编辑状态
const isEdit = computed(() => {
  return crud.status.edit > 0
})

const validateNumerical = (rule, value, callback) => {
  if (!form.minNumerical || !form.maxNumerical) {
    callback(new Error('请填写最小或最大数值'))
  }
  callback()
}

const rules = {
  classificationId: [{ required: true, message: '请选择分段类型', trigger: 'change' }],
  numerical: [{ required: true, validator: validateNumerical, message: '请填写数值', trigger: 'blur' }]
}

function handleClassificationChange() {
  for (const item in typeProcessObj.value) {
    form.processObj[item] = typeProcessObj.value[item]
  }
}

CRUD.HOOK.beforeSubmit = (crud, form) => {
  crud.form.boxTypeId = crud.query.id
  crud.form.structureProcessPriceList = obj2arr(crud.form.processObj)
}
</script>
