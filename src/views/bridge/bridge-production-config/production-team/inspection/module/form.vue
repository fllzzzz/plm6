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
          :disabledProductType="[componentTypeEnum.MACHINE_PART.V]"
          :size="'small'"
          :multiple="false"
          style="width: 270px"
          @change="processChange"
        />
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
import processSelect from '@/components-system/bridge/process-select'
import userSelect from '@comp-common/user-select'
import { artifactProductLineEnum } from '@enum-ms/mes'
import { bridgeProcessTypeEnum as componentTypeEnum } from '@enum-ms/bridge'

const formRef = ref()
const processSelectRef = ref()
const inspectorSelectRef = ref()
const productType = ref()
const defaultForm = {
  id: undefined,
  processId: undefined,
  productionLineTypeEnum: artifactProductLineEnum.TRADITION.V,
  inspectorIds: []
}

const { crud, form } = regForm(defaultForm, formRef)
const isEdit = computed(() => crud.status.edit >= 1)

const rules = {
  factoryId: [{ required: true, message: '请选择工厂', trigger: 'change' }],
  processId: [{ required: true, message: '请选择工序', trigger: 'change' }],
  inspectorIds: [{ required: true, message: '请选择质检', trigger: 'change' }]
}

function processChange(val) {
  productType.value = processSelectRef.value?.getOption(val)?.productType
}

function inspectorChange(userlist) {
  form.inspectors = userlist
}
</script>
