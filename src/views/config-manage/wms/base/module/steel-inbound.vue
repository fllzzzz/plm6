<template>
  <el-card shadow="always">
    <template #header>
      <div class="clearfix">
        <span class="card-title">钢材入库配置</span>
        <common-tip-button
          v-permission="permission.steelInboundEdit"
          :loading="submitLoading"
          :disabled="submitDisabled"
          show-tip
          size="mini"
          type="primary"
          style="float: right"
          @click="submit"
        >
          保存
        </common-tip-button>
      </div>
    </template>
    <el-form
      ref="formRef"
      v-loading="dataLoading"
      :disabled="formDisabled"
      :model="form"
      :rules="rules"
      label-position="top"
      label-width="170px"
    >
      <el-form-item class="form-tip-item" prop="trainsDiffType">
        <template #label>
          <span>车次钢材总重误差({{ form.trainsDiffType === numOrPctEnum.NUMBER.V ? STEEL_DIFF_UNIT : '%' }})</span>
        </template>
        <div class="flex-r">
          <common-radio-button v-model="form.trainsDiffType" :options="numOrPctEnum.ENUM" type="enum" size="small" />
          <common-input-number
            v-model="form.trainsDiff"
            :max="form.trainsDiffType === numOrPctEnum.NUMBER.V ? 999999999999 : 100"
            controls-position="right"
            size="small"
            placeholder="请输入允许的误差值"
            style="width: 200px; margin-left: 10px"
            :min="0"
            :precision="0"
          />
        </div>
      </el-form-item>
      <el-form-item>
        <span class="form-item-tip">
          可配置一车次钢材的最大误差。【入库钢材的总重量】与【当前车过磅重量】的差值（绝对值）超过该误差，将无法提交入库申请。
        </span>
        <span class="form-item-tip">固定重量({{ STEEL_DIFF_UNIT }})：误差不可超过 固定重量；</span>
        <span class="form-item-tip">百分比(%)：误差不可超过 入库钢材总重量*百分比。</span>
      </el-form-item>
      <el-form-item class="form-tip-item" label="重量允许误差(%)" prop="steelDiffType">
        <template #label>
          <span>单件钢材重量误差({{ form.steelDiffType === numOrPctEnum.NUMBER.V ? STEEL_DIFF_UNIT : '%' }})</span>
        </template>
        <div class="flex-r">
          <common-radio-button v-model="form.steelDiffType" :options="numOrPctEnum.ENUM" type="enum" size="small" />
          <common-input-number
            v-model="form.steelDiff"
            :max="form.steelDiffType === numOrPctEnum.NUMBER.V ? 999999999999 : 100"
            controls-position="right"
            size="small"
            placeholder="请输入允许的误差值"
            style="width: 200px; margin-left: 10px"
            :min="0"
          />
        </div>
      </el-form-item>
      <el-form-item>
        <span class="form-item-tip">
          可配置单件钢材的最大误差。【钢材的入库重量】与【钢材的理论重量】的差值（绝对值）超过该误差，办理时将发出预警（可提交入库申请）。
        </span>
        <span class="form-item-tip">固定重量({{ STEEL_DIFF_UNIT }})：误差不可超过 固定重量；</span>
        <span class="form-item-tip">百分比(%)：误差不可超过 入库钢材的理论重量*百分比。</span>
      </el-form-item>
      <el-form-item label="误差超过设定范围可提交">
        <el-switch
          v-model="form.overDiffSubmittable"
          :active-value="whetherEnum.TRUE.V"
          :inactive-value="whetherEnum.FALSE.V"
          class="drawer-switch"
        />
      </el-form-item>
      <el-form-item>
        <span class="form-item-tip">当误差超过设定范围时仍然可以提交入库单。</span>
      </el-form-item>
    </el-form>
  </el-card>
</template>

<script setup>
import { getInboundSteelConf, setInboundSteelConf } from '@/api/config/wms/base'
import { reactive, ref, onMounted, inject, computed } from 'vue'
import { STEEL_DIFF_UNIT } from '@/settings/config'
import { numOrPctEnum, whetherEnum } from '@enum-ms/common'
import { deepClone } from '@/utils/data-type'
import { isObjectValueEqual } from '@data-type/object'

import useWatchFormValidate from '@compos/form/use-watch-form-validate'
import { ElNotification } from 'element-plus'

const permission = inject('permission')

// 数据源
const dataSource = ref({
  // 车次重量差值
  trainsDiff: undefined,
  // 车次重量差值类型（g 或 %）
  trainsDiffType: undefined,
  // 单件钢材差值
  steelDiff: undefined,
  // 差值类型（g 或 %）
  steelDiffType: undefined,
  // 不在误差范围中是否可提交
  overDiffSubmittable: true
})

// from-dom
const formRef = ref()

// 表单
const form = ref(dataSource.value)

// loading
const dataLoading = ref(false)
const submitLoading = ref(false)

// 未修改时，禁止点击保存按钮
const submitDisabled = computed(() => isObjectValueEqual(form.value, dataSource.value))
const formDisabled = computed(() => dataLoading.value || submitLoading.value)

// 表单校验
const rules = reactive({
  trainsDiffType: [{ validator: validateTrainsDiff }],
  steelDiffType: [{ validator: validateSteelDiff }]
})

onMounted(() => {
  fetchData()
})

useWatchFormValidate(formRef, form)

// 加载数据
async function fetchData() {
  dataLoading.value = true
  try {
    const res = await getInboundSteelConf()
    form.value = deepClone(res)
    dataSource.value = res
  } catch (error) {
    console.log('获取入库钢材配置', error)
  } finally {
    dataLoading.value = false
  }
}

// 保存数据
async function submit() {
  try {
    const passed = await formRef.value.validate()
    if (!passed) return
    submitLoading.value = true
    await setInboundSteelConf(form.value)
    ElNotification({
      title: '入库钢材配置设置成功',
      type: 'success',
      duration: 2500
    })
    dataSource.value = deepClone(form.value)
    // TODO:更新配置
    // await store.dispatch('config/fetchConfigInfo')
  } catch (error) {
    ElNotification({
      title: '基础钢材配置设置失败',
      type: 'error',
      duration: 2500
    })
  } finally {
    submitLoading.value = false
  }
}

// 表单校验
function validateSteelDiff(rule, value, callback) {
  const flag = value === numOrPctEnum.PERCENTAGE.V && form.value.steelDiff > 100
  if (flag) {
    callback(new Error('误差百分比不可超过100'))
  }
  callback()
}

function validateTrainsDiff(rule, value, callback) {
  const flag = value === numOrPctEnum.PERCENTAGE.V && form.value.trainsDiff > 100
  if (flag) {
    callback(new Error('误差百分比不可超过100'))
  }
  callback()
}
</script>
