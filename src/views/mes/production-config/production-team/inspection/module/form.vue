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
    <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="100px">
      <el-form-item label="工厂" prop="factoryId">
        <factory-select :disabled="isEdit" v-model="form.factoryId" placeholder="请选择工厂" style="width: 270px" />
      </el-form-item>
      <el-form-item label="工序" prop="processId">
        <process-select
          ref="processSelectRef"
          v-model="form.processId"
          :disabledProductType="[componentTypeEnum.ENCLOSURE.V,componentTypeEnum.MACHINE_PART.V]"
          :size="'small'"
          :multiple="false"
          style="width: 270px"
          @change="processChange"
        />
      </el-form-item>
      <el-form-item v-if="productType === componentTypeEnum.ARTIFACT.V" label="生产线类型" prop="productionLineTypeEnum">
        <el-select v-model="form.productionLineTypeEnum" placeholder="请选择生产线类型" :size="'small'" style="width: 270px">
          <el-option v-for="item in artifactProductLineEnum.ENUM" :key="item.V" :label="item.L" :value="item.V" />
        </el-select>
      </el-form-item>
      <el-form-item label="质检" prop="inspectorIds">
        <user-select
          ref="inspectorSelectRef"
          v-model="form.inspectorIds"
          :multiple="true"
          placeholder="请选择质检人员"
          style="width: 270px"
          @change="inspectorChange"
        />
      </el-form-item>
    </el-form>
  </common-dialog>
</template>

<script setup>
import { ref, computed } from 'vue'
import { regForm } from '@compos/use-crud'
import factorySelect from '@comp-base/factory-select.vue'
import processSelect from '@comp-mes/process-select'
import userSelect from '@comp-common/user-select'
import { artifactProductLineEnum, componentTypeEnum } from '@enum-ms/mes'

const formRef = ref()
const processSelectRef = ref()
const inspectorSelectRef = ref()
const productType = ref()
const defaultForm = {
  id: undefined,
  productionLineTypeEnum: artifactProductLineEnum.TRADITION.V,
  processId: undefined,
  inspectorIds: []
}

const { crud, form } = regForm(defaultForm, formRef)
const isEdit = computed(() => crud.status.edit >= 1)

const rules = {
  factoryId: [{ required: true, message: '请选择工厂', trigger: 'change' }],
  productionLineTypeEnum: [{ required: true, message: '请选择生产线', trigger: 'change' }],
  processId: [{ required: true, message: '请选择工序', trigger: 'change' }],
  inspectorIds: [{ required: true, message: '请选择质检', trigger: 'change' }]
}

function processChange(val) {
  productType.value = processSelectRef.value?.getOption(val)?.productType
  if (productType.value !== componentTypeEnum.ARTIFACT.V) {
    form.productionLineTypeEnum = artifactProductLineEnum.TRADITION.V
  }
}

function inspectorChange(userlist) {
  form.inspectors = userlist
}
</script>
