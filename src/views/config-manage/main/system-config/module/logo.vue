<template>
  <el-card v-if="checkPermission(permission.get)" class="box-card" shadow="hover">
    <template #header class="clearfix">
        <el-tooltip
            class="item"
            effect="light"
            :content="`logo使用说明：\n
            1.请上传jpg、jpeg、png格式的图片。\n
            2.单张图片大小不能超过10MB。\n
            3.最多上传10张图片。\n
            4.部分打印会使用到默认logo。`"
            placement="top"
        >
        <div style="display:inline-block;cursor: pointer">
          <span class="title">公司logo配置</span>
          <i class="el-icon-info" style="color: #909399" />
        </div>
      </el-tooltip>
    </template>
    <div v-loading="loading">
      <el-upload
        :action="fileUploadCompanyLogoApi"
        accept="image/*"
        :limit="10"
        :headers="headers"
        list-type="picture-card"
        :file-list="list"
        :before-upload="handleBefore"
        :on-success="handleChange"
        :on-error="handleError"
        :on-exceed="handleExceed"
      >
        <i class="el-icon-plus" />
        <template #file="{ file }" style="height: 100%">
          <el-image
            class="el-upload-list__item-thumbnail"
            :src="file.path"
            alt="logo"
            fit="scale-down"
          />
          <span class="el-upload-list__item-actions">
            <span
              class="el-upload-list__item-preview"
              @click="handlePictureCardPreview(file)"
            >
              <i class="el-icon-zoom-in" />
            </span>
            <span
              v-if="checkPermission(permission.edit)"
              class="el-upload-list__item-delete"
              @click="handleCheck(file)"
            >
              <i class="el-icon-check" />
            </span>
            <span
              v-if="checkPermission(permission.edit)"
              class="el-upload-list__item-delete"
              @click="handleRemove(file)"
            >
              <i class="el-icon-delete" />
            </span>
          </span>
          <table-cell-tag v-if="file.isDefault" name="默认" color="#1890ff" />
        </template>
      </el-upload>
      <el-dialog v-model="dialogVisible">
        <img style="width: 100%" :src="dialogImageUrl" alt="logo">
      </el-dialog>
    </div>
  </el-card>
</template>

<script setup>
import { getLogoConfig, setLogoConfig, delLogoConfig } from '@/api/config/main/system-config'
import { ref } from 'vue'
import { mapGetters } from '@/store/lib'
import { useStore } from 'vuex'

import checkPermission from '@/utils/system/check-permission'
import { getToken } from '@/utils/storage'

import { systemConfigPM } from '@/page-permission/config'
import { ElUpload, ElMessage } from 'element-plus'

const { fileUploadCompanyLogoApi } = mapGetters('fileUploadCompanyLogoApi')
const permission = systemConfigPM.logo

const store = useStore()
const headers = ref({ Authorization: getToken() })
const loading = ref(false)
const dialogVisible = ref(false)
const dialogImageUrl = ref('')
const list = ref([])

fetch()

async function fetch() {
  if (!checkPermission(permission.get)) return
  loading.value = true
  try {
    const { content = [] } = await getLogoConfig() || {}
    list.value = content.map(v => {
      return {
        id: v.id,
        isDefault: v.isDefault,
        path: v.path
      }
    })
  } catch (error) {
    console.log('上传公司logo', error)
  } finally {
    loading.value = false
  }
}

function setCompany(data = {}) {
  store.dispatch('config/setCompany', setCompany)
}

async function handleRemove(file) {
  try {
    if (await delLogoConfig(file.id)) {
      const index = list.value.findIndex(v => v.id === file.id)
      let logo = ''
      if (index !== -1) {
        ElMessage.success('已删除')
        list.value.splice(index, 1)
        if (list.value.length && file.isDefault) {
          list.value[0].isDefault = true
          logo = list.value[0].path
        }
      }
      setCompany({ logo })
    }
  } catch (error) {
    console.log('删除公司logo', error)
  }
}
function handlePictureCardPreview(file) {
  dialogImageUrl.value = file.path
  dialogVisible.value = true
}
async function handleCheck(file) {
  if (!file.isDefault) {
    await setLogoConfig(file.id)
    list.value.forEach(v => {
      v.isDefault = v.id === file.id
      if (v.isDefault) {
        setCompany({ logo: v.path })
      }
    })
  }
}
function handleChange(res, file, fileList) {
  fileList.splice(fileList.length - 1, 1)
  if (res.code === 20000) {
    if (!list.value.length) {
      res.isDefault = true
      setCompany({ logo: res.path })
    }
    list.value.push(res.data)
    ElMessage.success('上传成功')
  } else {
    ElMessage.error(res.message)
  }
}
function handleError(err, file, fileList) {
  ElMessage.error(err.message)
}
function handleExceed() {
  ElMessage.error('最多只能上传10张logo')
}
function handleBefore(file) {
  if (file.size > 10 * 1024 * 1024) {
    ElMessage.error(`上传logo大小不能超过10MB!`, 2000)
    return false
  }
  return true
}
</script>

<style lang="scss" scoped>
.title{
    height: 32px;
    line-height: 32px;
    margin-right: 4px;
}
</style>
