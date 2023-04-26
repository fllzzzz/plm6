<template>
  <common-drawer
    append-to-body
    ref="drawerRef"
    v-model="visible"
    top="10vh"
    width="600px"
    :before-close="handleClose"
    title="文件详情"
    :wrapper-closable="false"
    size="900px"
    custom-class="contract-change"
  >
    <template #content>
      <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="150px">
        <el-form-item label="文件名称">
          <span></span>
        </el-form-item>
        <el-form-item label="文件">
          <span>{{ currentMonomer.name }}</span>
        </el-form-item>
        <el-row :gutter="20">
          <el-col :span="12">
            <el-form-item label="文件类型">
              <span>{{ currentRow.fileName }}</span>
            </el-form-item>
          </el-col>
          <el-col :span="12">
            <el-form-item label="文件属性">
              <span>{{ currentRow.fileName }}</span>
            </el-form-item>
          </el-col>
        </el-row>
        <el-form-item label="所属项目">
          <span>{{ currentMonomer.name }}</span>
        </el-form-item>
        <el-row :gutter="20">
          <el-col :span="12">
            <el-form-item label="上传人">
              <span></span>
            </el-form-item>
          </el-col>
          <el-col :span="12">
            <el-form-item label="上传日期">
            <span></span>
          </el-form-item>
          </el-col>
        </el-row>
        <el-form-item label="备注">
          <span></span>
        </el-form-item>
      </el-form>
      <el-divider><span class="title">历史修订版本</span></el-divider>
      <common-table
        ref="detailRef"
        border
        :data="[]"
        :max-height="300"
        style="width: 100%"
        class="table-form"
        return-source-data
      >
        <el-table-column label="序号" type="index" align="center" width="50" />
        <el-table-column prop="depositBank" label="版本" align="center" />
        <el-table-column prop="account" label="文件" align="center" min-width="270">
          <template v-slot="scope">
            <el-input v-model="scope.row.account" type="text" placeholder="账号" style="width: 260px" maxlength="30"/>
          </template>
        </el-table-column>
        <el-table-column prop="depositBank" label="上传人" align="center" />
        <el-table-column prop="depositBank" label="上传时间" align="center" />
        <el-table-column label="操作" align="center">
          <template v-slot="scope">
            <common-button size="small" class="el-icon-view" type="primary"/>
          </template>
        </el-table-column>
      </common-table>
    </template>
  </common-drawer>
</template>

<script setup>
import { defineProps, defineEmits, computed, ref, watch } from 'vue'
import useVisible from '@compos/use-visible'

const props = defineProps({
  currentMonomer: {
    type: Object,
    default: () => {}
  },
  globalProject: {
    type: Object,
    default: () => {}
  },
  dataType: {
    type: [String, Number],
    default: undefined
  },
  modelValue: {
    type: Boolean,
    require: true
  },
  currentRow: {
    type: Object,
    default: () => {}
  }
})

const defaultForm = {
  fileName: undefined
}

const form = ref(JSON.parse(JSON.stringify(defaultForm)))
const formRef = ref()
const rules = {
  fileName: { required: true, message: '请输入文件命名', trigger: 'blur' }
}
const emit = defineEmits(['success', 'update:modelValue'])
const { visible, handleClose } = useVisible({ emit, props })

watch(
  () => visible.value,
  (val) => {
    if (val) {
      form.value.fileName = undefined
    }
  },
  { deep: true, immediate: true }
)

const carryParam = computed(() => {
  return {}
  // return props.currentRow.id ? { id: props.currentRow.id, fileName: form.value.fileName } : { projectId: props.globalProject.id, monomerId: props.currentMonomer.id, dataType: props.dataType, fileName: form.value.fileName }
})

function handleSuccess() {
  emit('success')
  handleClose()
}

</script>
<style lang="scss" scoped>
::v-deep(.el-input-number .el-input__inner) {
  text-align: left;
}
</style>
