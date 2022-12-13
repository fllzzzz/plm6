<template>
  <el-card shadow="always">
    <template #header>
      <div class="clearfix">
        <span class="card-title">物料加权配置</span>
        <common-tip-button
          v-permission="permission.materialWeightingEdit"
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
    <el-form ref="formRef" v-loading="dataLoading" :disabled="formDisabled" :model="form" label-position="left" label-width="80px">
      <el-form-item label="加权方式">
        <div style="display: flex">
          <common-radio v-model="form.weightedType" :options="materialWeightingWayEnum.ENUM" type="enum" />
          <span
            style="color: red"
            v-if="(form.weightedType !== curConfig?.weightedType && !submitDisabled && !curConfig?.boolUpdate) || (curConfig?.boolUpdate && submitDisabled)"
          >
            （次月修改为：{{ materialWeightingWayEnum.VL[form.weightedType] }}）
          </span>
        </div>
      </el-form-item>
      <el-form-item label-width="0">
        <span class="form-item-tip">加权方式变更，在未发生物料入库时，即时生效。反之，次月（月末加权后）生效。</span>
        <span class="form-item-tip">全库加权：材料会以公司为维度进行加权平均计算，即相同材料在全库价格相同。</span>
        <span class="form-item-tip">单库加权：材料会以项目为维度进行加权平均的计算，即相同材料在公共库、不同项目库的价格不相同。</span>
        <span class="form-item-tip" style="margin-top: 25px">全库加权的影响（相对于单库加权）：</span>
        <span class="form-item-tip">1. 甲供材料调拨不提供“借用”与“买入”功能，与 "非甲供物料" 调拨处理方式相同。</span>
        <span class="form-item-tip">2. 存货明细账不体现调拨数据，且存货明细账只可按全库查询。</span>
        <span class="form-item-tip">3. 收发存报表查询时（未选择项目，即全库查询）会进行项目汇总。</span>
      </el-form-item>
    </el-form>
  </el-card>
</template>

<script setup>
import { getMaterialWeightingConf, setMaterialWeightingConf } from '@/api/config/wms/base'
import { ref, onMounted, inject, computed } from 'vue'
import { materialWeightingWayEnum } from '@enum-ms/wms'
import { deepClone } from '@/utils/data-type'

import useRefreshStore from '@/composables/store/use-refresh-store'
import { ElNotification } from 'element-plus'

const permission = inject('permission')

// 数据源
const dataSource = ref({
  weightedType: undefined // 加权方式
})

const curConfig = ref({})

// from-dom
const formRef = ref()

// 表单
const form = ref(dataSource.value)

// loading
const dataLoading = ref(false)
const submitLoading = ref(false)

// 未修改时，禁止点击保存按钮
const submitDisabled = computed(() => form.value?.weightedType === dataSource.value?.weightedType)
const formDisabled = computed(() => dataLoading.value || submitLoading.value)

onMounted(() => {
  fetchData()
})

// 加载数据
async function fetchData() {
  dataLoading.value = true
  try {
    const res = await getMaterialWeightingConf()
    curConfig.value = res
    if (res.boolUpdate) {
      res.weightedType =
        res.weightedType === materialWeightingWayEnum.WHOLE.V ? materialWeightingWayEnum.SINGLE.V : materialWeightingWayEnum.WHOLE.V
    }
    dataSource.value = res
    form.value = deepClone(res)
  } catch (error) {
    console.log('获取物料加权配置', error)
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
    await setMaterialWeightingConf({
      weightedType: form.value.weightedType
    })
    ElNotification({
      title: '物料加权配置设置成功',
      type: 'success',
      duration: 2500
    })
    const res = await getMaterialWeightingConf()
    curConfig.value = res
    if (res.boolUpdate) {
      res.weightedType =
        res.weightedType === materialWeightingWayEnum.WHOLE.V ? materialWeightingWayEnum.SINGLE.V : materialWeightingWayEnum.WHOLE.V
    }
    dataSource.value = res
    useRefreshStore('wmsConfig')
  } catch (error) {
    ElNotification({
      title: '物料加权配置设置失败',
      type: 'error',
      duration: 2500
    })
  } finally {
    submitLoading.value = false
  }
}
</script>
