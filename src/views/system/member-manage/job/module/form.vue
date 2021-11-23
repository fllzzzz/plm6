<template>
  <common-dialog
    append-to-body
    :close-on-click-modal="false"
    :before-close="crud.cancelCU"
    :visible="crud.status.cu > 0"
    :title="crud.status.title"
    width="600px"
  >
    <template #titleRight>
      <common-button :loading="crud.status.cu === 2" type="primary" size="mini" @click="crud.submitCU">确认</common-button>
    </template>
    <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="80px" :inline="true">
      <el-form-item
        label="名称"
        prop="name"
      >
        <el-input
          v-model="form.name"
          style="width: 370px;"
        />
      </el-form-item>
      <el-form-item
        label="排序"
        prop="sort"
      >
        <el-input-number
          v-model.number="form.sort"
          :min="0"
          :max="999"
          controls-position="right"
          style="width: 370px;"
        />
      </el-form-item>
      <el-form-item
        label="状态"
        prop="enabled"
      >
        <common-radio
          v-model="form.enabled"
          :options="systemEnabledEnum.ENUM"
          type="enum"
        />
      </el-form-item>
      <el-form-item label="所属部门" prop="pid">
        <el-popover
          placement="top-start"
          width="450"
          trigger="click"
          v-model:visible="menuVisible"
          ref="menuPopover"
        >
          <menuSelect ref="menuSelectRef" @selected="menuSelected" style="width:400px;" :pid="form.deptId" :defaultProps="{ children: 'children', label: 'name' }" :treeMenu="deptTreeData"/>
          <template #reference>
            <el-input v-model="form.deptName" style="width: 450px;" placeholder="点击选择所属部门" readonly />
          </template>
        </el-popover>
      </el-form-item>
    </el-form>
  </common-dialog>
</template>

<script setup>
import { ref } from 'vue'
import { regForm } from '@compos/use-crud'
import menuSelect from '@/components-system/common/tree-select.vue'
import { deptTree } from '@/api/system/member-manage/dept'
import { systemEnabledEnum } from '@enum-ms/system'
const formRef = ref()
const menuPopover = ref()
const menuSelectRef = ref()
const menuVisible = ref(false)
const deptTreeData = ref([])
const defaultForm = {
  id: undefined,
  name: '',
  sort: 1,
  enabled: systemEnabledEnum.TRUE.V,
  deptId: undefined,
  deptName: undefined
}
const { crud, form } = regForm(defaultForm, formRef)

const rules = {
  name: [
    { required: true, message: '请填写名称', trigger: 'blur' }
  ],
  sort: [
    { required: true, message: '请填写序号', trigger: 'blur', type: 'number' }
  ],
  deptId: { required: true, message: '所属部门不能为空', trigger: 'change' }
}

// 加载数据
fetchDeptTree()

// 拉取部门树
async function fetchDeptTree() {
  try {
    deptTreeData.value = await deptTree({ 'hasRoot': true })
  } catch (error) {
    console.log('部门tree', error)
  }
}

function menuSelected(value) {
  form.deptId = value.id
  form.deptName = value.name
  menuVisible.value = false
}
</script>
<style lang="scss" scoped>
  ::v-deep(.el-input-number .el-input__inner) {
    text-align: left;
  }
</style>
