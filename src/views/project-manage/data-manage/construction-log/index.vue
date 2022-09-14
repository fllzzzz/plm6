<template>
  <div class="app-container">
    <div style="height:32px;">
      <export-button
        :params="{yearMonth: activeMonth,projectId: globalProjectId }"
        :fn="download"
        style="float:right;"
        v-if="checkPermission(permission.download) && globalProject?.businessType===businessTypeEnum.INSTALLATION.V"
      >批量下载施工日志</export-button>
    </div>
    <el-config-provider :locale="locale">
      <el-calendar v-model="journal.date">
        <template
          #dateCell="{data}"
        >
          <div
            class="calendar-item"
            :class="[moment(data.date).valueOf() < activeDateEndTp && moment(data.date).valueOf()>=activeDateStartTp ?(isNotBlank(journal.map[data.day])?'has-content':'need'):'',data.isSelected ? 'is-selected' : '']"
            @click="openJournal(data.day)"
          >
            <span>{{ data.day.split('-').slice(1).join('-') }}</span>
            <div v-if="isNotBlank(journal.map[data.day])" style="margin-top:10px;">
              <div class="journal-info">{{ journal.map[data.day].morningWeather }} | {{ journal.map[data.day].afternoonWeather }}</div>
              <div class="journal-info">{{ journal.map[data.day].morningTemperature }} ℃ ~ {{ journal.map[data.day].afternoonTemperature }} ℃</div>
            </div>
          </div>
        </template>
      </el-calendar>
    </el-config-provider>
    <mDetail v-if="checkPermission(permission.detail)" v-model="journal.visible" :info="journal.info" :day="journal.day" :project-id="globalProjectId"  @success="fetchList" :permission="permission" :activeMonth="activeMonth"/>
  </div>
</template>

<script setup>
import crudApi, { download } from '@/api/project-manage/data-manage/construction-log'
import { reactive, computed, watch } from 'vue'

import { businessTypeEnum } from '@enum-ms/contract'
import { ElCalendar, ElConfigProvider } from 'element-plus'
import zhCn from 'element-plus/lib/locale/lang/zh-cn'
import { constructionLogPM as permission } from '@/page-permission/project'
import checkPermission from '@/utils/system/check-permission'
import moment from 'moment'
import { mapGetters } from '@/store/lib'
import { isNotBlank } from '@data-type/index'

import mDetail from './module/detail'
import ExportButton from '@comp-common/export-button/index.vue'

const locale = reactive(zhCn)

const { globalProjectId, globalProject } = mapGetters(['globalProjectId', 'globalProject'])

const journal = reactive({
  day: '',
  map: {},
  list: [],
  info: {},
  visible: false,
  date: new Date()
})

// 当前浏览月份
const activeMonth = computed(() => {
  return moment(journal.date).format('YYYY-MM')
})

watch(
  () => activeMonth.value,
  (newVal, oldVal) => {
    if (newVal === oldVal) return
    if (moment(activeMonth.value).valueOf() > moment().valueOf()) return
    fetchList()
  },
  { deep: true, immediate: true }
)
// 当前浏览月份 - 开始时间戳
const activeDateStartTp = computed(() => {
  return moment(activeMonth.value).startOf('month').valueOf()
})

// 当前浏览月份 - 结束时间戳
const activeDateEndTp = computed(() => {
  const today = moment().valueOf()
  const end = moment(activeMonth.value).endOf('month').valueOf()
  return end > today ? today : end
})

// 初始化
function initVal() {
  journal.list = []
  journal.map = {}
}

// 获取列表
async function fetchList() {
  initVal()
  if (globalProject.value?.businessType !== businessTypeEnum.INSTALLATION.V) {
    return
  }
  try {
    const query = {
      yearMonth: activeMonth.value,
      projectId: globalProjectId.value
    }
    const { content = [] } = await crudApi.get(query)
    journal.list = content.map(v => {
      v.day = moment(v.constructionDate).format('YYYY-MM-DD')
      journal.map[v.day] = v
      return v
    })
  } catch (error) {
    console.log('施工日志: ', error)
  }
}

// 打开施工日志
function openJournal(day) {
  if (globalProject.value?.businessType !== businessTypeEnum.INSTALLATION.V) {
    return
  }
  if (moment(day).valueOf() > moment().valueOf()) return
  journal.day = day
  journal.info = isNotBlank(journal.map[day]) && journal.map[day] || {}
  journal.visible = true
}
</script>

<style lang="scss" scoped>
::v-deep(.el-calendar-table  .el-calendar-day) {
  padding: 0;
}

.calendar-item{
  height: 100%;
  padding: 8px;

  .journal-info{
    font-size: 12px;
    margin:5px auto;
  }

}

.need{
  background-color:#fdf5e9;
}

.has-content{
  background-color:#b3e19d;
}

.is-selected {
  color: #1989FA;
}
</style>
