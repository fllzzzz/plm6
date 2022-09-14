<template>
  <common-drawer
    v-model="visible"
    size="50%"
    :show-close="false"
    :before-close="handleClose"
  >
  <template #title>
    <div class="dialog-title flex-rbc" >
      <div class="title">
        <span>施工日志</span>
      </div>
      <div>
        <export-button
        :params="{yearMonth: activeMonth,projectId: projectId, day:new Date(day).getDate()<10?'0'+new Date(day).getDate():new Date(day).getDate()}"
        :fn="download"
        v-if="checkPermission(permission.download) && !isEdit"
      >下载施工日志</export-button>
        <common-button size="mini" v-if="info.id && isEdit" @click="isEdit=false">取消</common-button>
        <common-button v-if="isEdit" :loading="loading" type="primary" size="mini" @click="submit">提交</common-button>
        <common-button v-if="checkPermission(permission.add) && !isEdit" size="mini" @click="isEdit=true" type="primary">修改</common-button>
        <common-button size="mini" @click="handleClose">关闭</common-button>
      </div>
    </div>
  </template>
  <template #content>
    <div class="dialog-container">
      <el-form ref="formRef" :model="form" :rules="rules" size="small" label-position="left" label-width="120px">
        <el-row :gutter="40">
          <el-col :span="11">
            <el-form-item label="日期">
              <span>{{ day }}</span>
            </el-form-item>
          </el-col>
        </el-row>
        <common-table
          ref="detailRef"
          border
          :data="form.links"
          style="width: 100%;margin-bottom:30px;"
          class="table-form"
          return-source-data
          :showEmptySymbol="false"
          :cell-class-name="wrongCellMask"
        >
          <el-table-column key="keyword" prop="keyword" label="时间" align="center">
            <template v-slot="scope">
              <span>{{ scope.row.time }}</span>
            </template>
          </el-table-column>
          <el-table-column key="weather" prop="weather" label="天气情况" align="center">
            <template v-slot="scope">
              <el-input
                v-model.trim="scope.row.weather"
                placeholder="天气情况"
                maxlength="20"
                size="small"
                class="input-underline"
                style="width:100%"
                :disabled="!isEdit"
              />
            </template>
          </el-table-column>
           <el-table-column key="wind" prop="wind" label="风力" align="center">
            <template v-slot="scope">
              <el-input
                v-model.trim="scope.row.wind"
                placeholder="风力"
                maxlength="20"
                size="small"
                class="input-underline"
                :disabled="!isEdit"
                style="width:100%"
              >
                <template v-slot:suffix>级</template>
              </el-input>
            </template>
          </el-table-column>
          <el-table-column key="temperature" prop="temperature" label="温度" align="center">
            <template v-slot="scope">
              <el-input
                v-model.trim="scope.row.temperature"
                placeholder="温度"
                maxlength="20"
                size="small"
                :disabled="!isEdit"
                class="input-underline"
                style="width:100%"
              >
                <template v-slot:suffix>℃</template>
              </el-input>
            </template>
          </el-table-column>
        </common-table>
        <el-form-item label="施工部分描述" prop="constructionDesc">
          <el-input
            v-if="isEdit"
            v-model.trim="form.constructionDesc"
            type="textarea"
            :autosize="{ minRows: 2, maxRows: 5}"
            placeholder="施工部分描述"
            style="width:90%"
            show-word-limit
            maxlength="500"
          />
          <span v-else>{{ form.constructionDesc  }}</span>
        </el-form-item>
        <el-form-item label="机械作业描述" prop="machineWorkDesc">
          <el-input
            v-if="isEdit"
            v-model.trim="form.machineWorkDesc"
            type="textarea"
            :autosize="{ minRows: 2, maxRows: 5}"
            placeholder="机械作业描述"
            style="width:90%"
            show-word-limit
            maxlength="500"
          />
          <span v-else>{{ form.machineWorkDesc  }}</span>
        </el-form-item>
        <el-form-item label="班组上岗情况" prop="groupWorkDesc">
          <el-input
            v-if="isEdit"
            v-model.trim="form.groupWorkDesc"
            type="textarea"
            :autosize="{ minRows: 2, maxRows: 5}"
            placeholder="班组上岗情况"
            style="width:90%"
            show-word-limit
            maxlength="500"
          />
          <span v-else>{{ form.groupWorkDesc  }}</span>
        </el-form-item>
        <el-form-item label="质量检查情况" prop="qualityInspectionDesc">
          <el-input
            v-if="isEdit"
            v-model.trim="form.qualityInspectionDesc"
            type="textarea"
            :autosize="{ minRows: 2, maxRows: 5}"
            placeholder="质量检查情况"
            style="width:90%"
            show-word-limit
            maxlength="500"
          />
          <span v-else>{{ form.qualityInspectionDesc  }}</span>
        </el-form-item>
        <el-form-item label="安全检查情况" prop="safetyInspectionDesc">
          <el-input
            v-if="isEdit"
            v-model.trim="form.safetyInspectionDesc"
            type="textarea"
            :autosize="{ minRows: 2, maxRows: 5}"
            placeholder="安全检查情况"
            style="width:90%"
            show-word-limit
            maxlength="500"
          />
          <span v-else>{{ form.safetyRecord  }}</span>
        </el-form-item>
        <el-form-item label="工地重大事件" prop="siteImportantEvent">
          <el-input
            v-if="isEdit"
            v-model.trim="form.siteImportantEvent"
            type="textarea"
            :autosize="{ minRows: 2, maxRows: 5}"
            placeholder="工地重大事件"
            style="width:90%"
            show-word-limit
            maxlength="500"
          />
          <span v-else>{{ form.siteImportantEvent  }}</span>
        </el-form-item>
        <el-form-item label="存在问题描述" prop="existingProblemDesc">
          <el-input
            v-if="isEdit"
            v-model.trim="form.existingProblemDesc"
            type="textarea"
            :autosize="{ minRows: 2, maxRows: 5}"
            placeholder="存在问题描述"
            style="width:90%"
            show-word-limit
            maxlength="500"
          />
          <span v-else>{{ form.existingProblemDesc  }}</span>
        </el-form-item>
        <el-row :gutter="40">
          <el-col :span="11">
            <el-form-item label="负责人" prop="managerId">
               <user-dept-cascader
                  v-if="isEdit"
                  v-model="form.managerId"
                  filterable
                  clearable
                  show-all-levels
                  placeholder="负责人"
                  style="width: 100%;"
                  class="input-underline"
                />
              <span v-else>{{ form.managerName  }}</span>
            </el-form-item>
          </el-col>
          <el-col :span="11">
            <el-form-item label="记录人" prop="recorderId">
               <user-dept-cascader
                  v-if="isEdit"
                  v-model="form.recorderId"
                  filterable
                  clearable
                  show-all-levels
                  placeholder="记录人"
                  style="width: 100%;"
                  class="input-underline"
                />
              <span v-else>{{ form.recorderName }}</span>
            </el-form-item>
          </el-col>
        </el-row>
      </el-form>
    </div>
  </template>

  </common-drawer>
</template>

<script setup>
import { watch, ref, defineEmits, defineProps, nextTick } from 'vue'
import crudApi, { download } from '@/api/project-manage/data-manage/construction-log'
import { ElNotification } from 'element-plus'
import useWatchFormValidate from '@compos/form/use-watch-form-validate'

import { constructionLogPM as permission } from '@/page-permission/project'
import checkPermission from '@/utils/system/check-permission'
import useTableValidate from '@compos/form/use-table-validate'
import useVisible from '@compos/use-visible'
import moment from 'moment'
import { isNotBlank } from '@/utils/data-type'

import userDeptCascader from '@comp-base/user-dept-cascader.vue'
import ExportButton from '@comp-common/export-button/index.vue'

const props = defineProps({
  modelValue: {
    type: Boolean,
    require: true
  },
  info: {
    type: Object,
    default: () => {}
  },
  day: {
    type: String,
    default: ''
  },
  activeMonth: {
    type: [String, Number],
    default: undefined
  },
  projectId: {
    type: [String, Number],
    default: void 0
  }
})

const defaultForm = {
  links: [],
  afternoonTemperature: undefined,
  afternoonWeather: undefined,
  afternoonWind: undefined,
  constructionDate: undefined,
  constructionDesc: undefined,
  existingProblemDesc: undefined,
  groupWorkDesc: undefined,
  id: undefined,
  machineWorkDesc: undefined,
  managerId: undefined,
  morningTemperature: undefined,
  morningWeather: undefined,
  morningWind: undefined,
  projectId: undefined,
  qualityInspectionDesc: undefined,
  recorderId: undefined,
  safetyInspectionDesc: undefined,
  siteImportantEvent: undefined
}

const form = ref(JSON.parse(JSON.stringify(defaultForm)))
const formRef = ref()
const loading = ref(false)

const rules = {
  managerId: [{ required: true, message: '请选择负责人', trigger: 'change' }],
  recorderId: [{ required: true, message: '请选择记录人', trigger: 'change' }]
}

const isEdit = ref(true)

const emit = defineEmits(['success', 'update:modelValue'])
const { visible, handleClose } = useVisible({ emit, props })

const tableRules = {
  weather: [{ required: true, message: '请输入天气', trigger: 'blur' }],
  wind: [{ required: true, message: '请输入风力', trigger: 'blur' }],
  temperature: [{ required: true, message: '请输入温度', trigger: 'blur' }]
}

const { tableValidate, wrongCellMask } = useTableValidate({ rules: tableRules }) // 表格校验

watch(() => visible.value, (val) => {
  if (val) {
    if (isNotBlank(props.info)) {
      resetForm(props.info)
      isEdit.value = false
    } else {
      resetForm()
      isEdit.value = true
    }
  }
})

function resetForm(data) {
  if (formRef.value) {
    formRef.value.resetFields()
  }
  if (data && Object.keys(data).length > 0) {
    form.value = data
    form.value.links = [
      { time: '上午', temperature: form.value.morningTemperature, weather: form.value.morningWeather, wind: form.value.morningWind },
      { time: '下午', temperature: form.value.afternoonTemperature, weather: form.value.afternoonWeather, wind: form.value.afternoonWind }
    ]
  } else {
    form.value = JSON.parse(JSON.stringify(defaultForm))
    form.value.links = [
      { time: '上午' },
      { time: '下午' }
    ]
  }
  if (formRef.value) {
    nextTick(() => {
      formRef.value.clearValidate()
    })
  }
  useWatchFormValidate(formRef, form)
}

// 刷新
function success() {
  emit('success')
}

// 提交
async function submit() {
  const { validResult, dealList } = tableValidate(form.value.links)
  if (validResult) {
    form.value.links = dealList
  } else {
    return validResult
  }
  try {
    loading.value = true
    const submitForm = {
      ...form.value,
      afternoonTemperature: form.value.links[1].temperature,
      afternoonWeather: form.value.links[1].weather,
      afternoonWind: form.value.links[1].wind,
      morningTemperature: form.value.links[0].temperature,
      morningWeather: form.value.links[0].weather,
      morningWind: form.value.links[0].wind,
      projectId: props.projectId,
      constructionDate: moment(props.day).valueOf()
    }
    if (props.info.id) {
      await crudApi.edit(submitForm)
    } else {
      await crudApi.add(submitForm)
    }
    ElNotification({
      title: props.info.id ? '修改成功' : '添加成功',
      type: 'success',
      duration: 2500
    })
    handleClose()
    success()
  } catch (error) {
    console.log('添加施工日志: ', error)
  } finally {
    loading.value = false
  }
}
</script>
