<template>
  <common-dialog
    append-to-body
    :close-on-click-modal="false"
    :visible="crud.status.cu > CRUD.STATUS.NORMAL"
    :before-close="crud.cancelCU"
    :title="`${crud.props.factory ? crud.props.factory.name + '：' : ''}${crud.status.title}`"
    :show-close="true"
    custom-class="conifg-wms-warehouse-form"
    width="600px"
  >
    <template #titleRight>
      <common-button :loading="crud.status.cu === CRUD.STATUS.PROCESSING" size="mini" type="primary" @click="crud.submitCU">确认</common-button>
    </template>
    <el-form
      ref="formRef"
      :model="form"
      :rules="rules"
      :disabled="crud.status.cu === CRUD.STATUS.PROCESSING"
      label-position="right"
      size="small"
      label-width="150px"
    >
      <el-form-item label="仓库位置" prop="name">
        <el-input v-model.trim="form.name" type="text" clearable placeholder="仓库位置" size="small" style="width: 100%" />
      </el-form-item>
      <el-form-item label="可存储材料类型" prop="materialType">
        <common-select
          v-model="form.materialType"
          :options="matClsEnum.ENUM"
          multiple
          type="enum"
          placeholder="可存储材料类型"
          style="width: 100%;"
        />
      </el-form-item>
      <el-form-item label="仓库类型" prop="type">
        <common-select
          v-model="form.type"
          :options="warehouseTypeEnum.ENUM"
          type="enum"
          placeholder="仓库类型"
          style="width: 100%;"
        />
      </el-form-item>
      <el-form-item label="排序" prop="sort">
        <common-input-number
          v-model="form.sort"
          controls-position="right"
          class="align-left"
          :max="99999"
          size="small"
          placeholder="排序"
          style="width: 100%;"
        />
      </el-form-item>
    </el-form>
  </common-dialog>
</template>

<script setup>
import { ref } from 'vue'
import EO from '@enum'
import { warehouseTypeEnum } from '@enum-ms/wms'
import { matClsEnum } from '@enum-ms/classification'

import { regForm } from '@compos/use-crud'

const defaultForm = {
  id: undefined,
  name: undefined,
  materialType: [],
  type: undefined,
  sort: undefined
}

// 校验规则
const rules = ref({
  name: [{ required: true, max: 20, message: '不能超过20个字符', trigger: 'blur' }],
  materialType: [{ required: true, message: '请选择仓库存储的材料类型', trigger: 'change' }],
  type: [{ required: true, message: '请选择仓库类型', trigger: 'change' }]
})

const formRef = ref()
const { CRUD, crud, form } = regForm(defaultForm, formRef)

crud.submitFormFormat = (form) => {
  form.materialType = EO.getBitsSum(form.materialType)
  form.factoryId = crud.query.factoryId
  return form
}
</script>

<style lang="scss" scoped>
.conifg-wms-warehouse-form {
  .align-left {
    ::v-deep(.el-input__inner) {
      text-align: left;
    }
  }
}
</style>
