<template>
  <el-card shadow="always">
    <template #header>
      <div class="clearfix">
        <span class="card-title">采购基础配置</span>
        <common-tip-button
          v-permission="permission.basicPurchaseEdit"
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
      label-position="left"
      label-width="130px"
    >
      <el-form-item label="采购信息是否可修改配置：" label-width="200px"></el-form-item>
      <el-form-item v-for="(item, key) in basicFillConfig" :key="key" :label="`【${rawMatClsEnum.VL[key]}】`" label-width="70px">
        <el-checkbox v-for="(spec, i) in item" :key="i" v-model="spec.V" :label="spec.L"></el-checkbox>
      </el-form-item>
      <!-- <el-form-item class="form-tip-item" label="重量允许误差(%)" prop="steelDiffType">
        <template #label>
          <span>钢材重量误差({{ form.steelDiffType === numOrPctEnum.NUMBER.V ? STEEL_DIFF_UNIT : '%' }})</span>
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
          可配置钢材的最大误差。【钢材的采购重量】与【钢材的申购重量】的差值（绝对值）超过该误差，办理时将发出预警（可提交采购申请）。
        </span>
        <span class="form-item-tip">固定重量({{ STEEL_DIFF_UNIT }})：误差不可超过 固定重量；</span>
        <span class="form-item-tip">百分比(%)：误差不可超过 采购钢材的申购重量*百分比。</span>
      </el-form-item>
      <el-form-item label="无申购采购重量误差：" label-width="153px">
        <span style="color: #e6a23cc2;">由【入库-单件钢材重量误差】配置决定</span>
      </el-form-item> -->
    </el-form>
  </el-card>
</template>

<script setup>
import { getPurchaseBasicConf, setPurchaseBasicConf } from '@/api/config/wms/base'
import { ref, onMounted, inject, computed, watchEffect } from 'vue'
// import { STEEL_DIFF_UNIT } from '@/settings/config'
// import { numOrPctEnum } from '@enum-ms/common'
import { rawMatClsEnum } from '@enum-ms/classification'
import { isObjectValueEqual } from '@data-type/object'
import { deepClone } from '@/utils/data-type'

import useRefreshStore from '@/composables/store/use-refresh-store'
import { ElNotification } from 'element-plus'

const permission = inject('permission')
const defaultConfig = {
  [rawMatClsEnum.STEEL_PLATE.V]: {
    spec: { L: '规格', V: false },
    thickness: { L: '厚度', V: false },
    length: { L: '长度', V: false },
    width: { L: '宽度', V: false },
    quantity: { L: '数量', V: false }
  },
  [rawMatClsEnum.SECTION_STEEL.V]: {
    spec: { L: '规格', V: false },
    length: { L: '定尺长度', V: false },
    quantity: { L: '数量', V: false }
  },
  [rawMatClsEnum.MATERIAL.V]: {
    quantity: { L: '数量', V: false },
    mete: { L: '核算量', V: false }
  }
}

const basicFillConfig = ref()

// 数据源
const dataSource = ref({
  // 单件钢材差值
  steelDiff: undefined,
  // 差值类型（g 或 %）
  steelDiffType: undefined
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
// const rules = reactive({
//   steelDiffType: [{ validator: validateSteelDiff }]
// })

watchEffect(() => {
  const config = {}
  for (const item in basicFillConfig.value) {
    for (const key in basicFillConfig.value[item]) {
      if (!config[key]) config[key] = 0
      if (basicFillConfig.value[item][key].V) {
        config[key] |= Number(item)
      }
    }
  }
  form.value = { ...form.value, ...config }
})

onMounted(() => {
  fetchData()
})

// 加载数据
async function fetchData() {
  dataLoading.value = true
  try {
    const res = await getPurchaseBasicConf()
    form.value = deepClone(res)
    dataSource.value = res
    basicFillConfig.value = defaultConfig
    for (const item in basicFillConfig.value) {
      for (const key in basicFillConfig.value[item]) {
        if (form.value[key] & item) {
          basicFillConfig.value[item][key].V = true
        }
      }
    }
  } catch (error) {
    console.log('wms基础配置', error)
  } finally {
    dataLoading.value = false
  }
}

// 保存数据
async function submit() {
  try {
    // const passed = await formRef.value.validate()
    // if (!passed) return
    submitLoading.value = true
    await setPurchaseBasicConf(form.value)
    ElNotification({
      title: '采购基础配置设置成功',
      type: 'success',
      duration: 2500
    })
    dataSource.value = deepClone(form.value)
    // fetchData()
    useRefreshStore('wmsConfig')
  } catch (error) {
    ElNotification({
      title: '采购基础配置设置失败',
      type: 'error',
      duration: 2500
    })
  } finally {
    submitLoading.value = false
  }
}

// 表单校验
// function validateSteelDiff(rule, value, callback) {
//   const flag = value === numOrPctEnum.PERCENTAGE.V && form.value.steelDiff > 100
//   if (flag) {
//     callback(new Error('误差百分比不可超过100'))
//   }
//   callback()
// }
</script>
