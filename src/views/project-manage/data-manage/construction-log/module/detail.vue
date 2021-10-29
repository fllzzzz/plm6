<template>
  <el-drawer
    v-model="drawerVisible"
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
        <common-button v-if="isEdit" :loading="detail.loading" type="primary" size="mini" @click="submit">提 交</common-button>
        <common-button size="mini" @click="handleClose">退 出</common-button>
      </div>
    </div>
  </template>
  <div class="dialog-container">
    <el-form ref="form" :model="detail"  size="small" label-position="left" label-width="155px">
      <el-row :gutter="40">
        <el-col :span="11">
          <el-form-item label="日期">
            <span>{{ day }}</span>
          </el-form-item>
        </el-col>
        <el-col :span="11">
          <el-form-item label="记录人" prop="createUserId">
            <!-- <user-dept-cascader
              v-if="isEdit"
              v-model:value="detail.info.createUserId"
              filterable
              size="mini"
              class="input-underline"
              clearable
              show-all-levels
              style="width:100%"
              placeholder="记录人"
            />
            <span v-else>{{ detail.info.createUserName  }}</span> -->
          </el-form-item>
        </el-col>
      </el-row>
      <el-row :gutter="40">
        <el-col :span="11">
          <el-form-item label="天气情况（上午）" prop="morningWeather">
            <el-input
              v-if="isEdit"
              v-model.trim="detail.info.morningWeather"
              placeholder="上午天气情况"
              maxlength="60"
              size="small"
              class="input-underline"
              style="width:100%"
            />
            <span v-else>{{ detail.info.morningWeather  }}</span>
          </el-form-item>
        </el-col>
        <el-col :span="11">
          <el-form-item label="天气情况（下午）" prop="afternoonWeather">
            <el-input
              v-if="isEdit"
              v-model.trim="detail.info.afternoonWeather"
              placeholder="下午天气情况"
              maxlength="60"
              size="small"
              class="input-underline"
              style="width:100%"
            />
            <span v-else>{{ detail.info.afternoonWeather  }}</span>
          </el-form-item>
        </el-col>
      </el-row>
      <el-row :gutter="40">
        <el-col :span="11">
          <el-form-item label="最低温度" prop="minTemperature">
            <el-input
              v-if="isEdit"
              v-model.trim="detail.info.minTemperature"
              placeholder="最低温度"
              maxlength="60"
              size="small"
              class="input-underline"
              style="width:100%"
            >
              <template v-slot:suffix>℃</template>
            </el-input>
            <span v-else>{{ detail.info.minTemperature  }} ℃</span>
          </el-form-item>
        </el-col>
        <el-col :span="11">
          <el-form-item label="最高温度" prop="maxTemperature">
            <el-input
              v-if="isEdit"
              v-model.trim="detail.info.maxTemperature"
              placeholder="最高温度"
              maxlength="60"
              size="small"
              class="input-underline"
              style="width:100%"
            >
              <template v-slot:suffix>℃</template>
            </el-input>
            <span v-else>{{ detail.info.maxTemperature  }} ℃</span>
          </el-form-item>
        </el-col>
      </el-row>
      <el-form-item label="生产情况记录" prop="productionRecord">
        <el-input
          v-if="isEdit"
          v-model.trim="detail.info.productionRecord"
          type="textarea"
          :autosize="{ minRows: 10, maxRows: 15}"
          placeholder="生产情况记录"
          style="width:90%"
        />
        <span v-else>{{ detail.info.productionRecord  }}</span>
      </el-form-item>
      <el-form-item label="技术、质量、安全记录" prop="safetyRecord">
        <el-input
          v-if="isEdit"
          v-model.trim="detail.info.safetyRecord"
          type="textarea"
          :autosize="{ minRows: 10, maxRows: 15}"
          placeholder="技术、质量、安全记录"
          style="width:90%"
        />
        <span v-else>{{ detail.info.safetyRecord  }}</span>
      </el-form-item>
    </el-form>
  </div>
  </el-drawer>
</template>

<script setup>
import { computed, watch, ref, inject, reactive, defineEmits, defineProps, getCurrentInstance } from 'vue'
import { ElNotification } from 'element-plus'
import moment from 'moment'

const { proxy } = getCurrentInstance()

const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  info: {
    type: Object,
    default: () => {}
  },
  day: {
    type: String,
    default: ''
  },
  projectId: {
    type: [String, Number],
    default: void 0
  }
})

const crudApi = inject('crudApi')

const detail = reactive({
  loading: false,
  info: {}
})

const isEdit = ref(false)

const drawerVisible = computed({
  get() {
    return props.visible
  },
  set() {
    return props.visible
  }
})

watch(() => props.visible, (val) => {
  if (val) {
    detail.info = { ...props.info }
    isEdit.value = proxy.$isBlank(detail.info)
  }
})

const emit = defineEmits(['update:visible', 'refresh'])

// 关闭
function handleClose() {
  emit('update:visible', false)
}

// 刷新
function refresh() {
  emit('refresh')
}

// 提交
async function submit() {
  if (proxy.$isBlank(detail.info)) {
    ElNotification({
      title: '请填写数据',
      type: 'error',
      duration: 2500
    })
    return
  }
  try {
    detail.loading = true
    const query = {
      ...detail.info,
      projectId: props.projectId,
      subTime: moment(props.day).valueOf()
    }
    await crudApi.add(query)
    ElNotification({
      title: '添加成功',
      type: 'success',
      duration: 2500
    })
    handleClose()
    refresh()
  } catch (error) {
    console.log('添加施工日志: ', error)
  } finally {
    detail.loading = false
  }
}
</script>
