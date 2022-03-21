<template>
  <common-dialog
    append-to-body
    :close-on-click-modal="false"
    :before-close="crud.cancelCU"
    :visible="crud.status.cu > 0"
    :title="crud.status.title"
    :show-close="false"
    width="30%"
    top="10vh"
  >
    <template #titleRight>
      <span style="float: right">
        <common-button :loading="crud.status.cu === CRUD.STATUS.PROCESSING" size="mini" type="primary" @click="crud.submitCU">
          提 交
        </common-button>
        <store-operation type="crud" />
        <common-button size="mini" @click="crud.cancelCU">关 闭</common-button>
      </span>
    </template>
    <div class="form">
      <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="100px" class="demo-form">
        <el-form-item label="机器编号：" prop="machineNumber">
          <el-input ref="saveTagInput" v-model="form.machineNumber" class="input-underline" placeholder="输入机器编号" />
        </el-form-item>
        <el-form-item label="机器名称：" prop="machineName">
          <el-input ref="saveTagInput" v-model="form.machineName" placeholder="输入机器名称" class="input-underline" />
        </el-form-item>
        <el-form-item label="机器类型：" prop="machineType">
          <el-input ref="saveTagInput" v-model="form.machineType" class="input-underline" placeholder="输入机器类型" />
        </el-form-item>
        <el-form-item label="车间信息：" prop="workshopInf">
          <el-input ref="saveTagInput" v-model="form.workshopInf" class="input-underline" placeholder="输入车间信息" />
        </el-form-item>
        <el-form-item label="位置：" prop="position">
          <el-input ref="saveTagInput" v-model="form.position" class="input-underline" placeholder="输入位置" />
        </el-form-item>
        <el-form-item label="负责人：" prop="director">
          <el-input ref="saveTagInput" v-model="form.director" class="input-underline" placeholder="输入负责人" />
        </el-form-item>
        <el-form-item label="工控机地址" prop="opcUrl">
          <el-input ref="saveTagInput" v-model="form.opcUrl" class="input-underline" placeholder="输入工控机地址" />
        </el-form-item>
        <el-form-item label="MAC地址：" prop="mac">
          <el-input ref="saveTagInput" v-model="form.mac" class="input-underline" placeholder="输入MAC地址" />
        </el-form-item>
        <el-form-item label="代号：" prop="code">
          <el-input ref="saveTagInput" v-model="form.code" class="input-underline" placeholder="输入代号" />
        </el-form-item>
        <el-form-item label="设备品牌：" prop="brand">
          <el-input ref="saveTagInput" v-model="form.brand" class="input-underline" placeholder="输入设备品牌" />
        </el-form-item>
      </el-form>
    </div>
  </common-dialog>
</template>

<script setup>
import { ref } from 'vue'
import { regForm } from '@compos/use-crud'
import storeOperation from '@crud/STORE.operation'

const formRef = ref()

const defaultForm = {
  machineNumber: '', // 机器编号
  machineName: '', // 机器名称
  machineType: '', // 机器类型
  workshopInf: '', // 车间信息
  position: '', // 位置
  director: '', // 负责人
  opcUrl: '', // 工控机地址
  brand: '',
  mac: '',
  code: ''
}

const { crud, form, CRUD } = regForm(defaultForm, formRef)

const rules = {
  machineNumber: [{ required: true, message: '请输入机器编号', trigger: 'blur' }],
  machineName: [{ required: true, message: '请输入机器名称', trigger: 'blur' }],
  machineType: [{ required: true, message: '请输入机器类型', trigger: 'blur' }],
  workshopInf: [{ required: true, message: '请输入车间信息', trigger: 'blur' }],
  position: [{ required: true, message: '请输入位置', trigger: 'blur' }],
  mac: [{ required: true, message: '请输入MAC地址', trigger: 'blur' }],
  opcUrl: [{ required: true, message: '请输入工控机地址', trigger: 'blur' }],
  director: [{ required: true, message: '请输入负责人', trigger: 'blur' }]
}

CRUD.HOOK.beforeToEdit = async () => { }

// 提交前
CRUD.HOOK.beforeSubmit = async () => { }
</script>

<style lang="scss" scoped></style>
