<template>
  <div class="app-container">
    <el-row :gutter="20">
      <el-col :xs="24" :sm="24" :md="8" :lg="6" :xl="5" style="margin-bottom: 10px">
        <el-card class="box-card">
          <template #header class="clearfix">
            <span>个人信息</span>
          </template>
          <div>
            <div style="text-align: center">
              <img src="@/assets/header.jpg" class="avatar">
            </div>
            <ul class="user-info">
              <li> 用户姓名 <div class="user-right">{{ user.name }}</div></li>
              <li> 员工编号 <div class="user-right">{{ user.username }}</div></li>
              <li> 手机号码 <div class="user-right">{{ user.phone }}</div></li>
              <li> 用户邮箱 <div class="user-right">{{ user.email }}</div></li>
              <li> 所属部门 <div class="user-right"> {{ user.dept }} <span v-if="user.job">/ {{ user.job }}</span></div></li>
              <li>
                 安全设置
                <div class="user-right">
                  <a @click="pwdVisible = true">修改密码</a>
                </div>
              </li>
            </ul>
          </div>
        </el-card>
      </el-col>
      <el-col :xs="24" :sm="24" :md="16" :lg="18" :xl="19">
        <!--    用户资料    -->
        <el-card class="box-card">
          <el-tabs>
            <el-tab-pane label="用户资料">
              <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="65px">
                <el-form-item label="姓名" prop="name">
                  <el-input v-model="form.name" style="width: 35%" />
                  <span style="color: #C0C0C0;margin-left: 10px;">用户姓名不作为登录使用</span>
                </el-form-item>
                <el-form-item label="手机号" prop="phone">
                  <el-input v-model="form.phone" style="width: 35%;" />
                  <span style="color: #C0C0C0;margin-left: 10px;">手机号码不能重复</span>
                </el-form-item>
                <el-form-item label="性别" prop="sex">
                  <el-radio-group v-model="form.sex" style="width: 178px">
                    <el-radio v-for="item in userSexEnum.ENUM" :key="item.V" :label="item.V">{{ item.L }}</el-radio>
                  </el-radio-group>
                </el-form-item>
                <el-form-item v-if="formChanged" label="">
                  <common-button :loading="saveLoading" size="mini" type="primary" @click="doSubmit">保存配置</common-button>
                  <common-button size="small" type="warning" @click="formInit">取消修改</common-button>
                </el-form-item>
              </el-form>
            </el-tab-pane>
            <el-tab-pane label="公司资料">
              <el-form size="small" label-width="75px">
                <el-form-item label="公司域名">
                  <div class="reg-company-box">
                    <el-input v-model="companyUrl" placeholder="请填写公司域名" class="input-with-select">
                      <template #prepend>
                        <el-select v-model="protocol" placeholder="请选择">
                          <el-option label="https://" value="https://" />
                          <el-option label="http://" value="http://" />
                        </el-select>
                      </template>
                    </el-input>
                  </div>
                </el-form-item>
                <el-form-item v-if="urlChanged">
                  <el-popover
                    v-model:visible="saveCompanyUrlVisible"
                    placement="bottom"
                    :width="180"
                  >
                    <p>确认更换公司域名?</p><p>修改完成后需要重新登录</p>
                    <div style="text-align: right; margin: 0">
                      <common-button size="mini" type="text" @click="saveCompanyUrlVisible = false">取消</common-button>
                      <common-button type="primary" size="mini" @click="saveCompanyUrl">确定</common-button>
                    </div>
                    <template #reference>
                      <common-button :loading="saveCompanyUrlLoading" size="mini" type="primary">保存配置</common-button>
                    </template>
                  </el-popover>
                  <common-button size="small" type="warning" @click="urlInit">取消修改</common-button>
                </el-form-item>
              </el-form>
            </el-tab-pane>
          </el-tabs>
        </el-card>
      </el-col>
    </el-row>
    <update-pwd ref="pwdRef" v-model="pwdVisible" />
  </div>
</template>
<script setup>
import { editUser } from '@/api/user'
import { ref, computed } from 'vue'
import store from '@/store'
import { mapGetters } from '@/store/lib'
import { useRouter } from 'vue-router'

import { userSexEnum } from '@enum-ms/system'
import { isObjectValueEqual } from '@data-type/object'
import { validURI } from '@/utils/validate'
import { validatorPhone } from '@/utils/validate/pattern'
import { getRequestUrl } from '@/utils/storage' // get token from cookie

import UpdatePwd from './module/update-pwd'
import { ElTabs, ElRadioGroup, ElTabPane, ElMessage } from 'element-plus'

const { user } = mapGetters('user')

const rules = {
  name: [
    { required: true, message: '请填写用户姓名', trigger: 'blur' },
    { min: 2, max: 20, message: '长度在 2 到 20 个字符', trigger: 'blur' }
  ],
  phone: [
    { required: true, message: '请填写手机号', trigger: 'blur' },
    { pattern: validatorPhone, message: '请填写正确的手机号', trigger: 'blur' }
  ]
}

const formRef = ref()
const pwdRef = ref()
const router = useRouter()
const pwdVisible = ref(false)
const saveLoading = ref(false)
const sourceData = ref({
  id: user.value?.id,
  name: user.value?.name,
  sex: user.value?.sex,
  phone: user.value?.phone
})
const form = ref({})
const saveCompanyUrlVisible = ref(false)
const saveCompanyUrlLoading = ref(false)
const protocol = ref('https://')
const companyUrl = ref('')

const requestUrl = getRequestUrl()

const formChanged = computed(() => {
  return !isObjectValueEqual(form.value, sourceData.value)
})

const fullUrl = computed(() => {
  return protocol.value + companyUrl.value
})

const urlChanged = computed(() => {
  return fullUrl.value !== requestUrl
})

formInit()
urlInit()

function urlInit() {
  const urlArr = requestUrl.split('://')
  protocol.value = urlArr[0] + '://'
  companyUrl.value = urlArr[1]
}

async function saveCompanyUrl() {
  try {
    if (validURI(fullUrl.value)) {
      if (fullUrl.value === requestUrl) {
        ElMessage.warning('未对公司域名进行修改')
      } else {
        ElMessage.success('修改成功，请重新登录')
        store.dispatch('user/setRequestUrl', fullUrl.value)
        setTimeout(() => {
          store.dispatch('user/logout')
          router.push(`/`)
          setTimeout(() => {
            location.reload()// 为了重新实例化vue-router对象 避免bug
          }, 300)
        }, 1000)
      }
    } else {
      ElMessage.warning('错误格式的域名')
    }
  } catch (error) {
    console.log(error)
  } finally {
    saveCompanyUrlVisible.value = false
  }
}

function formInit() {
  form.value = { ...sourceData.value }
  formRef.value && formRef.value.clearValidate()
}

function doSubmit() {
  if (formRef.value) {
    formRef.value.validate((valid) => {
      if (valid) {
        saveLoading.value = true
        editUser(form.value).then(() => {
          ElMessage.success('修改成功')
          sourceData.value = { ...form.value }
          store.dispatch('user/getInfo').then(() => {})
          saveLoading.value = false
        }).catch(() => {
          saveLoading.value = false
        })
      }
    })
  }
}
</script>

<style rel="stylesheet/scss" lang="scss">
.avatar {
  width: 120px;
  height: 120px;
  display: block;
  border-radius: 50%
}

.el-form {
  min-height: 200px;
}

.user-info {
  padding-left: 0;
  list-style: none;
  li{
    border-bottom: 1px solid #F0F3F4;
    padding: 11px 0;
    font-size: 13px;
  }
  .user-right {
    float: right;
    a{
      color: #317EF3;
    }
  }
}

.reg-company-box {
  width: 100%;

  .el-select .el-input {
    width: 100px;
  }

  .input-with-select {
    .el-input-group__prepend {
      background-color: #fff;
    }
  }
}
</style>
