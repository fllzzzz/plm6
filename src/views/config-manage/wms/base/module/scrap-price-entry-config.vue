<template>
  <el-card shadow="always">
    <template #header>
      <div class="clearfix">
        <span class="card-title">废料价格录入</span>
        <common-tip-button
          style="float: right"
          size="mini"
          type="primary"
          :loading="submitLoading"
          show-tip
          @click="submit"
          :disabled="submitDisabled"
          v-permission="permission.scrapPriceEdit"
        >
          保存
        </common-tip-button>
        <!-- v-permission="permission.partyABorrowReturnEdit"

 -->
      </div>
    </template>
    <el-form>
      <el-form-item label="录入时机">
        <common-radio v-model="enterType" :options="scrapPriceTypeEnum.ENUM" type="enum" size="small" @change="enterChange" />
      </el-form-item>
    </el-form>
  </el-card>
</template>
<script setup>
import { ref, onMounted, inject } from 'vue'
import { scrapPriceTypeEnum } from '@enum-ms/wms'
import { getScrapPriceConf, setScrapPriceConf } from '@/api/config/wms/base'
import { ElNotification } from 'element-plus'
// import { isObjectValueEqual } from '@data-type/object'
import useRefreshStore from '@/composables/store/use-refresh-store'

const dataSource = ref(undefined)
const permission = inject('permission')

const enterType = ref(dataSource.value)

// loading
const dataLoading = ref(false)
const submitLoading = ref(false)

// 未修改时，禁止点击保存按钮
const submitDisabled = ref(true)

onMounted(() => {
  fetchData()
})

// 获取录入价格配置
const fetchData = async () => {
  dataLoading.value = true
  try {
    const res = await getScrapPriceConf()
    console.log(res)
    enterType.value = res
    dataSource.value = res
  } catch (error) {
    console.log('wms基础配置', error)
  } finally {
    dataLoading.value = false
  }
}

const enterChange = (v) => {
  enterType.value = v
  if (v === dataSource.value) {
    submitDisabled.value = true
  } else {
    submitDisabled.value = false
  }
}

const submit = async () => {
  try {
    submitLoading.value = true

    await setScrapPriceConf({ type: enterType.value })
    ElNotification({
      title: '废料价格录入设置成功',
      type: 'success',
      duration: 2500
    })
    useRefreshStore('wmsConfig')
    submitDisabled.value = true
    fetchData()
  } catch (error) {
    ElNotification({
      title: '废料价格录入设置失败',
      type: 'error',
      duration: 2500
    })
  } finally {
    submitLoading.value = false
  }
}

</script>
