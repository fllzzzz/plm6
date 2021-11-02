<template>
  <div class="app-container">
    <el-config-provider :locale="locale">
      <el-calendar v-model="journal.date">
        <template
          #dateCell="{data}"
        >
          <div
            class="calendar-item"
            :class="[!$isNotBlank(journal.map[data.day]) && moment(data.date).valueOf() < activeDateEndTp && moment(data.date).valueOf()>=activeDateStartTp ?'need':'',data.isSelected ? 'is-selected' : '']"
            @click="openJournal(data.day)"
          >
            <span>{{ data.day.split('-').slice(1).join('-') }}</span>
            <div v-if="$isNotBlank(journal.map[data.day])" style="margin-top:10px;">
              <div class="journal-info">{{ journal.map[data.day].morningWeather }} | {{ journal.map[data.day].afternoonWeather }}</div>
              <div class="journal-info">{{ journal.map[data.day].minTemperature }} ℃ ~ {{ journal.map[data.day].maxTemperature }} ℃</div>
            </div>
          </div>
        </template>
      </el-calendar>
    </el-config-provider>
    <mDetail v-if="useCheckPermission(permission.detail)" v-model:visible="journal.visible" :info="journal.info" :isEdit="journal.isEdit" :day="journal.day" :project-id="globalProjectId" @refresh="fetchList" />
  </div>
</template>

<script setup>
import { ElCalendar, ElConfigProvider } from 'element-plus'
import zhCn from 'element-plus/lib/locale/lang/zh-cn'
import crudApi from '@/api/project/data-manage/construction-log'
import { reactive, provide, computed, getCurrentInstance } from 'vue'
import useCheckPermission from '@compos/use-check-permission'
import moment from 'moment'
import { mapGetters } from '@/store/lib'
import mDetail from './module/detail'

// 权限
const permission = {
  get: ['project_constructionLog:get'],
  add: ['project_constructionLog:add'],
  detail: ['project_constructionLog:detail']
}

provide('permission', permission)
provide('crudApi', crudApi)

const locale = reactive(zhCn)

const { globalProjectId } = mapGetters(['globalProjectId'])

const { proxy } = getCurrentInstance()

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

fetchList()

// 初始化
function initVal() {
  journal.list = []
  journal.map = {}
}

// 获取列表
async function fetchList() {
  if (!useCheckPermission(permission.get)) return
  initVal()
  try {
    const startDate = moment(activeMonth.value).startOf('month').valueOf()
    const endDate = moment(activeMonth.value).endOf('month').valueOf()
    const query = {
      endDate,
      startDate,
      projectId: globalProjectId
    }
    const { content = [] } = await crudApi.get(query)
    journal.list = content.map(v => {
      v.day = moment(v.subTime).format('YYYY-MM-DD')
      journal.map[v.day] = v
      return v
    })
  } catch (error) {
    console.log('施工日志: ', error)
  }
}

// 打开施工日志
function openJournal(day) {
  if (moment(day).valueOf() > moment().valueOf()) return
  journal.day = day
  journal.info = proxy.$isNotBlank(this.journal.map[day]) && this.journal.map[day] || {}
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

.is-selected {
  color: #1989FA;
}
</style>
