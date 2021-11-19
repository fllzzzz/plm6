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
      <el-form-item label="员工编号" prop="username">
        <el-input v-model="form.username" :readonly="form.id?true:false" />
      </el-form-item>
      <el-form-item label="电话" prop="phone">
        <el-input v-model.number="form.phone" />
      </el-form-item>
      <el-form-item label="姓名" prop="name">
        <el-input v-model="form.name" />
      </el-form-item>
      <el-form-item label="邮箱" prop="email">
        <el-input v-model="form.email" />
      </el-form-item>
      <el-form-item label="部门" prop="deptId">
        <el-popover
          placement="bottom-start"
          width="450"
          trigger="click"
          v-model:visible="menuVisible"
          ref="menuPopover"
        >
          <menuSelect ref="menuSelectRef" @selected="menuSelected" style="width:400px;" :pid="form.deptId" :treeMenu="props.deptTree" :defaultProps="{ children: 'children', label: 'name' }"/>
          <template #reference>
            <el-input v-model="form.deptName" style="width: 450px;" placeholder="点击选择所属部门" readonly />
          </template>
        </el-popover>
      </el-form-item>
      <el-form-item label="岗位" prop="jobId">
        <el-select v-model="form.jobId" :loading="jobLoading" clearable style="width: 185px" placeholder="请先选择部门">
          <el-option
            v-for="item in jobs"
            :key="item.id"
            :label="item.name"
            :value="item.id"
          />
        </el-select>
      </el-form-item>
      <el-form-item label="性别">
         <common-radio
          v-model="form.sex"
          :options="userSexEnum.ENUM"
          type="enum"
        />
      </el-form-item>
      <el-form-item label="状态">
         <el-radio-group v-model="form.enabled" style="width: 178px" :disabled="form.id === user.id">
          <el-radio v-for="item in enabledEnum.ENUM" :key="item.V" :label="item.V">{{ item.L }}</el-radio>
        </el-radio-group>
      </el-form-item>
      <el-form-item style="margin-bottom: 0;" label="角色" prop="roles">
        <el-select
          v-model="form.roleIds"
          style="width: 460px"
          multiple
          placeholder="请选择"
          :loading="rolesLoading.value"
        >
          <el-option
            v-for="(item,index) in roles"
            :key="`${index}`"
            :label="item.name"
            :value="item.id"
          />
        </el-select>
      </el-form-item>
    </el-form>
  </common-dialog>
</template>

<script setup>
import { defineProps, ref } from 'vue'
import { regForm } from '@compos/use-crud'
import { roleAll } from '@/api/system/member-manage/role'
import { jobAll } from '@/api/system/member-manage/job'
import menuSelect from '@/components-system/system/tree-select/index.vue'
import { userSexEnum } from '@enum-ms/system'
import { enabledEnum } from '@enum-ms/common'
import { mapGetters } from '@/store/lib'
import { validatorPhone, validatorUsername } from '@/utils/validate/pattern'
import { isNotBlank } from '@data-type/index'
const { user } = mapGetters(['user'])

const formRef = ref()
const menuPopover = ref()
const menuSelectRef = ref()
const menuVisible = ref(false)
const jobs = ref([])
const roles = ref([])
const rolesLoading = ref(false)
const jobLoading = ref(false)
const choseMenu = ref({})
const props = defineProps({
  deptTree: {
    type: Array,
    default: () => []
  }
})
const defaultForm = {
  id: undefined,
  username: undefined,
  name: undefined,
  sex: userSexEnum.MALE.V,
  email: undefined,
  roles: [],
  roleIds: [],
  jobId: undefined,
  deptId: undefined,
  deptName: undefined,
  phone: undefined,
  enabled: enabledEnum.TRUE.V
}
const { CRUD, crud, form } = regForm(defaultForm, formRef)

const rules = {
  username: [
    { required: true, message: '请填写员工编号', trigger: 'blur' },
    { pattern: validatorUsername.pattern, message: validatorUsername.message, trigger: 'blur' }
  ],
  name: [
    { required: true, message: '请填写用户姓名', trigger: 'blur' },
    { min: 2, max: 20, message: '长度在 2 到 20 个字符', trigger: 'blur' }
  ],
  email: [
    { type: 'email', message: '请填写正确的邮箱地址', trigger: 'blur' }
  ],
  phone: [
    { required: true, message: '请填写手机号', trigger: 'blur' },
    { pattern: validatorPhone, message: '请填写正确的手机号', trigger: 'blur' }
  ]
}

function menuSelected(value) {
  choseMenu.value = value
  form.deptId = value.id
  form.deptName = value.name
  getJobAll(value.id)
  menuVisible.value = false
}

getRoleAll()

async function getRoleAll() {
  try {
    const { content } = await roleAll()
    roles.value = content
  } catch (e) {
    console.log('获取角色', e)
  }
}

getJobAll()

async function getJobAll(id) {
  try {
    const submitData = { deptId: id }
    const { content } = await jobAll(submitData)
    jobs.value = content
  } catch (e) {
    console.log('获取角色', e)
  }
}

let allMenu = []
function arrFn(source) {
  source.forEach(el => {
    allMenu.push(el)
    el.children && el.children.length > 0 ? arrFn(el.children) : ''
  })
}
// 打开编辑弹窗前做的操作
CRUD.HOOK.afterToEdit = (crud, form) => {
  form.roleIds = form.roles.map(r => r.id)
  form.jobId = form.job && form.job.id
  form.deptId = form.dept && form.dept.id
  if (isNotBlank(form.deptId)) {
    getJobAll(form.deptId)
    allMenu = []
    arrFn(props.deptTree)
    const value = allMenu.find(v => v.id === form.deptId)
    form.deptName = value.name
  }
}
</script>
<style lang="scss" scoped>
  ::v-deep(.el-input-number .el-input__inner) {
    text-align: left;
  }
</style>
