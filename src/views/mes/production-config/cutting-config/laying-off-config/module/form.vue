<template>
  <common-dialog
    append-to-body
    :close-on-click-modal="false"
    :before-close="crud.cancelCU"
    :visible="crud.status.cu > 0"
    :title="crud.status.title"
    :show-close="false"
    custom-class="mes-cutting-config"
    width="30%"
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
      <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="100px" class="demo-form">
        <!-- <el-form-item label="下料类别：" prop="materialType">
          <common-select
            ref="materialTypeRef"
            type="enum"
            v-model="form.materialType"
            :options="materialTypeEnum.ENUM"
            clearable
            placeholder="请选择下料类别"
            style="width: 270px"
            class="filter-item"
          />
        </el-form-item> -->
        <el-form-item prop="name" label="下料方式：">
          <el-input v-model="form.name" :maxlength="8" class="filter-item" placeholder="输入下料方式" style="width: 270px" />
        </el-form-item>
        <!-- <el-form-item prop="taskPrefix" label="任务前缀：">
          <el-input v-model="form.taskPrefix" class="filter-item" placeholder="输入任务前缀" style="width: 270px" />
        </el-form-item> -->
      </el-form>
    </div>
  </common-dialog>
</template>

<script setup>
import { ref } from 'vue'
import { regForm } from '@compos/use-crud'
// import { materialTypeEnum } from '@enum-ms/uploading-form'

const formRef = ref()

const defaultForm = {
  materialType: '', // 下料类别
  name: '' // 下料方式
  // taskPrefix: '' // 任务前缀
}

const { crud, form, CRUD } = regForm(defaultForm, formRef)

const rules = {
  materialType: [{ required: true, message: '请选择下料类别', trigger: 'blur' }],
  name: [
    { required: true, message: '请输入下料方式', trigger: 'blur' },
    { max: 8, message: '不能超过8个字符', trigger: 'blur' }
  ]
  // taskPrefix: [{ required: true, pattern: /^[A-Z]*$/, message: '请输入大写字母', trigger: 'blur' }]
}
</script>

<style lang="scss" scoped></style>
