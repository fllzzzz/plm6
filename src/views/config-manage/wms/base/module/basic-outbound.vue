<template>
  <el-card shadow="always">
    <template #header>
      <div class="clearfix">
        <span class="card-title">基础出库配置</span>
        <common-tip-button
          v-permission="permission.basicOutboundEdit"
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
    <el-form v-loading="dataLoading" :disabled="formDisabled" :model="form" label-position="top" label-width="120px">
      <el-form-item label="辅材入库到车间直接出库">
        <el-switch
          v-model="form.boolAuxMatToWorkShopWay"
          :active-value="whetherEnum.TRUE.V"
          :inactive-value="whetherEnum.FALSE.V"
          class="drawer-switch"
        />
      </el-form-item>
       <el-form-item>
        <span class="form-item-tip">在辅材入库到类型为“车间”的仓库时，无需办理，直接出库。</span>
      </el-form-item>

      <el-form-item label="气体即入即出">
        <el-switch
          v-model="form.boolGasOutAfterInbound"
          :active-value="whetherEnum.TRUE.V"
          :inactive-value="whetherEnum.FALSE.V"
          class="drawer-switch"
        />
      </el-form-item>
       <el-form-item>
        <span class="form-item-tip">“气体”在通过入库审核后，无需办理，直接出库。</span>
      </el-form-item>

      <el-form-item label="项目仓物料可出库到其他项目">
        <el-switch
          v-model="form.boolCanOutToOtherProject"
          :active-value="whetherEnum.TRUE.V"
          :inactive-value="whetherEnum.FALSE.V"
          class="drawer-switch"
        />
      </el-form-item>
       <el-form-item>
        <span class="form-item-tip">在项目仓中可以选择原材料出库到其他项目, 在仓管确认出库后，会生成已审核的物料调拨单（审核人为确认出库的用户）。</span>
      </el-form-item>
    </el-form>
  </el-card>
</template>

<script setup>
import { getOutboundBasicConf, setOutboundBasicConf } from '@/api/config/wms/base'
import { ref, onMounted, inject, computed } from 'vue'
import { whetherEnum } from '@enum-ms/common'
import { deepClone } from '@/utils/data-type'
import { isObjectValueEqual } from '@data-type/object'
import { ElNotification } from 'element-plus'

const permission = inject('permission')

// 数据源
const dataSource = ref({
  // 辅材出库到车间的配置
  boolAuxMatToWorkShopWay: undefined,
  // 气体入库直接出库
  boolGasOutAfterInbound: undefined,
  // 项目仓可以出库给其他项目
  boolCanOutToOtherProject: undefined
})
// 表单
const form = ref(dataSource.value)

// loading
const dataLoading = ref(false)
const submitLoading = ref(false)

// 未修改时，禁止点击保存按钮
const submitDisabled = computed(() => isObjectValueEqual(form.value, dataSource.value))
const formDisabled = computed(() => dataLoading.value || submitLoading.value)

onMounted(() => {
  fetchData()
})

// 加载数据
async function fetchData() {
  dataLoading.value = true
  try {
    const { boolAuxMatToWorkShopWay, boolGasOutAfterInbound, boolCanOutToOtherProject } = await getOutboundBasicConf()

    form.value = { boolAuxMatToWorkShopWay, boolGasOutAfterInbound, boolCanOutToOtherProject }
    dataSource.value = { boolAuxMatToWorkShopWay, boolGasOutAfterInbound, boolCanOutToOtherProject }
  } catch (error) {
    console.log('出库基础配置', error)
  } finally {
    dataLoading.value = false
  }
}

// 保存数据
async function submit() {
  try {
    submitLoading.value = true

    await setOutboundBasicConf(form.value)
    ElNotification({
      title: '出库基础配置设置成功',
      type: 'success',
      duration: 2500
    })

    dataSource.value = deepClone(form.value)
    // TODO:更新配置
    // await store.dispatch('config/fetchConfigInfo')
  } catch (error) {
    ElNotification({
      title: '出库基础配置设置失败',
      type: 'error',
      duration: 2500
    })
  } finally {
    submitLoading.value = false
  }
}
</script>
