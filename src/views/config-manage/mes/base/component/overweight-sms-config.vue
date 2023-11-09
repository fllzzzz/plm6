<template>
  <el-card shadow="always">
    <template #header>
      <div class="clearfix">
        <span class="card-title">过磅短信配置</span>
        <div style="float: right">
          <common-button v-permission="permission.overweightSMSRecipientEdit" size="mini" type="primary" v-if="!isEdit" @click="handleEdit">
            编辑
          </common-button>
          <template v-else>
            <common-button size="mini" type="warning" @click="cancel" style="margin-right: 6px">取消编辑</common-button>
            <common-tip-button :loading="submitLoading" show-tip size="mini" type="primary" @click="submit"> 保存 </common-tip-button>
          </template>
        </div>
      </div>
    </template>
    <common-table
      v-loading="dataLoading"
      :data="smsList"
      return-source-data
      :cell-class-name="wrongCellMask"
      :show-empty-symbol="false"
      style="margin-bottom: 20px"
    >
      <el-table-column prop="workshopIds" label="车间">
        <template #header>
          <el-tooltip
            class="item"
            effect="light"
            :content="`1.不选择车间时默认为所有车间。
            2.只能有一条全部车间的配置。`"
            placement="right"
          >
            <div style="display: inline-block; cursor: pointer">
              <span class="card-title">车间</span>
              <i class="el-icon-info" style="color: #909399; margin-left: 5px" />
            </div>
          </el-tooltip>
        </template>
        <template #default="{ row, $index }">
          <workshop-select
            v-if="isEdit"
            :key="Math.random()"
            v-model="row.workshopIds"
            :disabledVal="
              smsList
                .filter((item, index) => index !== $index)
                .map((item) => item.workshopIds)
                .flat(Infinity)
            "
            size="mini"
            multiple
            clearable
            placeholder="全部车间"
            style="width: 100%"
          />
          <span v-else>{{ row.workshopNames }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="id" label="短信接收人" align="center" width="116">
        <template #default="{ row }">
          <user-select
            v-if="isEdit"
            v-model="row.id"
            size="mini"
            placeholder="短信接收人"
            style="width: 100%"
            @change="handleUserChange($event, row)"
          />
          <span v-else>{{ row.name }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="phone" label="手机号码" align="center" width="112">
        <template #default="{ row }">
          <el-input v-if="isEdit" v-model="row.phone" size="mini" maxlength="11" placeholder="手机号码" style="width: 100%" />
          <span v-else>{{ row.phone }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="isEdit" label="操作" width="80" align="center">
        <template #default="{ $index }">
          <common-button
            size="mini"
            v-if="$index + 1 === smsList.length && $index + 1 < max"
            type="primary"
            icon="el-icon-plus"
            @click="add"
            style="padding: 6px"
          ></common-button>
          <common-button size="mini" type="danger" icon="el-icon-delete" @click="del($index)" style="padding: 6px"></common-button>
        </template>
      </el-table-column>
    </common-table>
    <el-form ref="formRef" v-loading="dataLoading" :model="form" label-width="100px">
      <el-form-item label="过磅超标重量允许值" prop="maxWeight" label-width="150px">
        <el-input-number
          v-if="checkPermission(permission.overweightSMSRecipientEdit) && isEdit"
          v-model.number="form.maxWeight"
          :min="0"
          :max="100"
          :step="1"
          :precision="2"
          placeholder="比例"
          :controls="false"
          style="width: 80px"
          class="input-underline"
        />
        <span v-else>{{ form.maxWeight }}</span>
        %
      </el-form-item>
    </el-form>
  </el-card>
</template>

<script setup>
import { getOverweightSMSRecipient as getConfig, setOverweightSMSRecipient as setConfig } from '@/api/config/mes/base'
import { ref, onMounted, inject } from 'vue'

import { deepClone } from '@/utils/data-type'
import { isNotBlank } from '@data-type/index'
import { validatorPhone } from '@/utils/validate/pattern'
import checkPermission from '@/utils/system/check-permission'

import useTableValidate from '@compos/form/use-table-validate'
import userSelect from '@comp-common/user-select'
import workshopSelect from '@comp-base/workshop-select'
import { ElNotification, ElMessage } from 'element-plus'

const permission = inject('permission')

const tableRules = {
  id: [{ required: true, message: '请选择过磅短信接收人', trigger: 'change' }],
  phone: [
    { required: true, message: '请填写手机号', trigger: 'blur' },
    { pattern: validatorPhone, message: '请填写正确的手机号', trigger: 'blur' }
  ]
}

const { tableValidate, cleanUpData, wrongCellMask } = useTableValidate({ rules: tableRules })

const formRef = ref()
const isEdit = ref(false)
const smsList = ref([])
const max = ref(4) // 最多可以配置4条数据
// 数据源
const dataSource = ref({
  userChecks: [],
  maxWeight: undefined
})
// 表单
const form = ref(dataSource.value)
// loading
const dataLoading = ref(false)
const submitLoading = ref(false)

onMounted(() => {
  fetchData()
})

async function fetchData() {
  dataLoading.value = true
  try {
    const { userChecks, maxWeight } = await getConfig()
    smsList.value = userChecks.map((row) => {
      row.workshopIds = row.workshops?.map((workshops) => workshops?.id)
      row.workshopNames = row.workshops?.map((workshops) => workshops?.name).join('、')
      if (!row.workshopNames) {
        row.workshopNames = '全部车间'
      }
      return row
    })
    form.value = { userChecks, maxWeight: isNotBlank(maxWeight) ? maxWeight : undefined }
    dataSource.value = { userChecks: deepClone(smsList.value), maxWeight: isNotBlank(maxWeight) ? maxWeight : undefined }
  } catch (error) {
    console.log('获取过磅超标短信接收人', error)
  } finally {
    dataLoading.value = false
  }
}

function handleEdit() {
  if (smsList.value.length === 0) {
    add()
  }
  isEdit.value = true
}

function cancel() {
  smsList.value = deepClone(dataSource.value.userChecks)
  form.value.maxWeight = dataSource.value.maxWeight
  isEdit.value = false
}

function del(index) {
  smsList.value.splice(index, 1)
}

function add() {
  smsList.value.push({
    id: undefined,
    name: '',
    phone: '',
    workshopIds: []
  })
}

function handleUserChange(val, row) {
  row.phone = val.phone
}

async function submit() {
  try {
    const { validResult, dealList } = tableValidate(smsList.value)
    if (validResult) {
      cleanUpData(dealList)
      if (dealList.filter((row) => row.workshopIds?.length === 0).length > 1) {
        ElMessage.error('最多只能保留一条全部车间的配置')
        return false
      }
      form.value.userChecks = dealList.map((row) => {
        row.workshops = row.workshopIds?.map((id) => {
          return { id }
        })
        return row
      })
      submitLoading.value = true
      await setConfig(form.value)
      ElNotification({
        title: '过磅超标短信接收人配置成功',
        type: 'success',
        duration: 2500
      })
      isEdit.value = false
      fetchData()
    }
  } catch (error) {
    console.log('设置过磅超标短信接收人', error)
  } finally {
    submitLoading.value = false
  }
}
</script>

<style lang="scss" scoped>
::v-deep(.el-input) {
  .el-input__inner {
    padding-left: 5px;
    padding-right: 5px;
  }
}
</style>
